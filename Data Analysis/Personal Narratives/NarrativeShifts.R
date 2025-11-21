#Modeling and visualizing shifts in personal narrative causal language 
#Manuscript: Network structure shapes collective narrative dynamics through individual decisions
#Script Author: Hunter Priniski (direct all hate mail to priniski@ucla.edu)

library(readxl)
require(brms)
require(rstan)
require(Rcpp)
require(scales)
require(gridExtra)
require(tidyverse)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

library(dplyr)
library(tidyverse)
library(bayesplot)

setwd("/Users/hunter/Desktop/PNAS/Data Analysis/Personal Narratives")


all_tweets <- read.csv('all_tweets.csv') #all tweets produced by participants
claims <- read.csv('claims.csv') #causal claims extracted from the Causal Claims Pipeline

claims_full <- inner_join(claims, all_tweets, by = c("text" = "Response"), 
                          relationship = "many-to-many")

counts_df <- claims_full %>%
  group_by(Structure, Content, Size, Phase, random.ID) %>%
  count()

#get the random.IDS of participants who didn't produce a tweet in a given phase
missing_ids <- all_tweets %>%
  anti_join(counts_df, by = c("Structure", "Content", "Size", "Phase", "random.ID")) %>%
  select(Structure, Content, Size, Phase, random.ID) %>%
  mutate(n = 0)

# Add missing rows to counts_df
counts_df <- counts_df %>%
  bind_rows(missing_ids)

View(counts_df)

#Difference modeling, calculating the shift in the amount of causal language used (postinteraction counts - pre counts)
dif_df <- counts_df %>%
  group_by(Structure, Content, Size, random.ID) %>%
  reframe(difference = n[Phase == "post"] - n[Phase == "pre"]) 

dif_df %>%
  group_by(Structure, Content)%>%
  summarize(
    positive_differences = sum(difference > 0, na.rm = TRUE),
    negative_differences = sum(difference < 0, na.rm = TRUE)
  )

# Summarize positive and negative differences by group
dif_df_summary <- dif_df %>%
  group_by(Structure, Content) %>%
  summarize(
    positive_differences = sum(difference > 0, na.rm = TRUE),
    zero_differences = sum(difference == 0, na.rm = TRUE), 
    negative_differences = sum(difference < 0, na.rm = TRUE),
    total_differences = negative_differences + zero_differences + positive_differences
  )


dif_df <- dif_df %>%
  left_join(dif_df_summary, by = c("Structure", "Content"))

#Produce Sup Mat Figure 7
ggplot(dif_df, aes(x = difference)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  facet_grid(Structure ~ Content) +
  labs(
    x = "Difference Score",
    y = "Count"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 18, face = "bold"),  # Axis titles
    axis.text = element_text(size = 16),                  # Axis text
    legend.title = element_text(size = 18, face = "bold"),# Legend title
    legend.position = "bottom",                           # Legend position
    legend.text = element_text(size = 16),                # Legend text
    strip.text = element_text(size = 18)                  # Facet label text
  ) +
  geom_text(
    data = dif_df_summary,
    aes(
      x = Inf, y = Inf,
      label = paste(
        "Neg:", negative_differences,
        "\nZero:", zero_differences,
        "\nPos:", positive_differences,
        "\nTotal:", total_differences
      )
    ),
    hjust = 1.1, vjust = 1.1, size = 4, color = "black"
  )


stats_df <- dif_df %>%
  group_by(Structure, Content) %>%
  summarize(mean_difference = mean(difference, na.rm = FALSE),
            sd_difference = sd(difference, na.rm = FALSE),
            n_difference = n(), 
            se_difference = sd_difference / sqrt(n_difference))

stats_df

#Figure 3B. Narrative shift plot in the manuscript
ggplot(stats_df, aes(x = Content, y = mean_difference, fill = Structure)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_difference - se_difference, ymax = mean_difference + se_difference),
                width = 0.2,                    # Width of the error bars
                position = position_dodge(0.9)) + # Ensure error bars align with bars
  labs(
    x = "Content",
    y = "Mean difference in number of \ncausal claims following interaction",
    fill = "Structure"
  ) + 
  theme_bw() +
  scale_fill_manual(values = c("Spatial" = "#E41A1C", "Homogeneous" = "#377EB8")) +
  theme(
    axis.title = element_text(size = 18, face = "bold"),  # Axis titles
    axis.text = element_text(size = 16),                  # Axis text
    legend.title = element_text(size = 18, face = "bold"),# Legend title
    legend.position = "bottom",                           # Legend position
    legend.text = element_text(size = 16),                # Legend text
    strip.text = element_text(size = 18)                  # Facet label text
  ) +
  ylim(-.01, 0.45)

#difference in causal language for each condition
t_test_result <- dif_df %>%
  group_by(Content,Structure) %>%
  summarise(t_test_p_value = t.test(difference)$p.value,
            t_test_statistic = t.test(difference)$statistic, 
            t_test_df = t.test(difference)$parameter)

#T test results statistics
t_test_result


####
#CLAIM LEVEL ANALYSIS
###

#These topic labels were taken from the GUI based version of the causal claims model. 
#Topics follow from Clustering the Embeddings from the Causal Claims Transformer model.
topic_labels <- c(
  '-1' = "No Cluster",
  '0' = "Nuclear Disaster",
  '1' = "Earthquake",
  '2' = "Energy Movement", #change to Energy Movement
  '3' = "Radiation",
  '4' = "Disaster",
  '5' = "Effects",
  '6' = "Tsunami",
  '7' = "Damage",
  '8' = "Issues",
  '9' = "Change (Loss)",
  '10' = "Health Issues",
  '11' = "Waves",
  '12' = "Terrible Event",
  '13' = "Destruction",
  '14' = "Displacement",
  '15' = "Earthquake",
  '16' = "Incident",
  '17' = "Natural Disaster",
  '18' = "Tsunami*",
  '19' = "Cancer"
)

#binomial test to compute the shift across each of the causal relations identified by the causal language algorithim
claims_full <- claims_full %>%
  unite(Identifier, Structure, Content, Size, random.ID, sep = "_", remove = FALSE)


# Map topic labels and apply post-processing. The clustering approach is unsupervised and can produce redunant topics. We map to same concepts here.
claims_full <- claims_full %>%
  mutate(
    # Map cluster IDs to topics using topic_labels
    cause_topic = topic_labels[as.character(cause_cluster)],
    effect_topic = topic_labels[as.character(effect_cluster)],
  ) %>%
  # Collapse redundant topics
  mutate(
    cause_topic = case_when(
      cause_topic %in% c("Earthquake") ~ "Earthquake",
      cause_topic %in% c("Tsunami", "Tsunami*", "Waves") ~ "Tsunami",
      cause_topic %in% c("Nuclear Disaster", 'Damage') ~ "Nuclear Disaster",
      cause_topic %in% c("Health Issues", "Cancer") ~ "Health Issues" ,
      TRUE ~ cause_topic
    ),
    effect_topic = case_when(
      effect_topic %in% c("Earthquake") ~ "Earthquake",
      effect_topic %in% c("Tsunami", "Tsunami*", "Waves") ~ "Tsunami",
      effect_topic %in% c("Nuclear Disaster", 'Damage') ~ "Nuclear Disaster",
      effect_topic %in% c("Health Issues", "Cancer") ~ "Health Issues" ,
      TRUE ~ effect_topic
    )
  )

claim_counts <- claims_full %>%
  group_by(Identifier, Phase, cause_topic, effect_topic)%>%
  summarise(count = n(), .groups = 'drop')

claim_counts2 <- claim_counts %>%
  complete(Identifier, Phase, cause_topic, effect_topic, 
           fill = list(count = 0))


#original does it by cause_cluster, effect_cluster; updated (remapped) does on _topic
shift_df <- claim_counts2 %>%
  group_by(Identifier, cause_topic, effect_topic) %>%
  reframe(difference = count[Phase == "post"] - count[Phase == "pre"]) 


shift_df <- shift_df %>%
  separate(Identifier, into = c("Structure", "Content", "Size", "random.ID"), sep = "_")


#T - test #make a t test, which will be simply difference scores, run a 1-sample t-test compared to difference = 0. just keep the ones who exhibited a change, for the claim level we can use the 621 subjects.
library(broom)

# Perform the t-test and collect results
shift_t_tests <- shift_df %>%
  group_by(Structure, Content, cause_topic, effect_topic) %>%
  summarize(
    t_test = list(t.test(difference, alternative = "two.sided", mu = 0)),
    .groups = "drop"
  ) %>%
  mutate(
    t_test_results = map(t_test, ~ broom::tidy(.x))
  ) %>%
  unnest(t_test_results) %>%
  select(
    Structure, Content, cause_topic, effect_topic,
    estimate, p.value, conf.low, conf.high
  )

#filter signifiant relations
sig_shift_t_tests <- shift_t_tests %>%
  filter(p.value < .05)

View(sig_shift_t_tests)



#Apply false discovery rate.

#For applying the FDR, we only consider possible relations that are expressed by the causal model for corrections
#g1 - g4 represent the corrections for each of the conditions. 
g1 <- shift_t_tests %>%
  filter(Content == 'Hashtag', Structure == 'Homogeneous')%>%
  filter(
    (cause_topic == "Earthquake" & effect_topic == "Tsunami") |
      (cause_topic == "Tsunami" & effect_topic == "Nuclear Disaster") |
      (cause_topic == "Nuclear Disaster" & effect_topic == "Change (Loss)") |
      (cause_topic == "Change (Loss)" & effect_topic == "Energy Movement") |
      (cause_topic == "Nuclear Disaster" & effect_topic == "Radiation") |
      (cause_topic == "Radiation" & effect_topic == "Health Issues") |
      (cause_topic == "Natural Disaster" & effect_topic == "Nuclear Disaster") |
      (cause_topic == "Tsunami" & effect_topic == "Destruction") 
  )%>%
  drop_na()


g2 <- shift_t_tests %>%
  filter(Content == 'Hashtag', Structure == 'Spatial')%>%
  filter(
    (cause_topic == "Earthquake" & effect_topic == "Tsunami") |
      (cause_topic == "Tsunami" & effect_topic == "Nuclear Disaster") |
      (cause_topic == "Nuclear Disaster" & effect_topic == "Change (Loss)") |
      (cause_topic == "Change (Loss)" & effect_topic == "Energy Movement") |
      (cause_topic == "Nuclear Disaster" & effect_topic == "Radiation") |
      (cause_topic == "Radiation" & effect_topic == "Health Issues") |
      (cause_topic == "Natural Disaster" & effect_topic == "Nuclear Disaster") |
      (cause_topic == "Tsunami" & effect_topic == "Destruction")
  )%>%
  drop_na()



g3 <- shift_t_tests %>%
  filter(Content == 'Face', Structure == 'Homogeneous')%>%
  filter(
    (cause_topic == "Earthquake" & effect_topic == "Tsunami") |
      (cause_topic == "Tsunami" & effect_topic == "Nuclear Disaster") |
      (cause_topic == "Nuclear Disaster" & effect_topic == "Change (Loss)") |
      (cause_topic == "Change (Loss)" & effect_topic == "Energy Movement") |
      (cause_topic == "Nuclear Disaster" & effect_topic == "Radiation") |
      (cause_topic == "Radiation" & effect_topic == "Health Issues") |
      (cause_topic == "Natural Disaster" & effect_topic == "Nuclear Disaster") |
      (cause_topic == "Tsunami" & effect_topic == "Destruction")
  )%>%
  drop_na()

g4 <- shift_t_tests %>%
  filter(Content == 'Face', Structure == 'Spatial')%>%
  filter(
    (cause_topic == "Earthquake" & effect_topic == "Tsunami") |
      (cause_topic == "Tsunami" & effect_topic == "Nuclear Disaster") |
      (cause_topic == "Nuclear Disaster" & effect_topic == "Change (Loss)") |
      (cause_topic == "Change (Loss)" & effect_topic == "Energy Movement") |
      (cause_topic == "Nuclear Disaster" & effect_topic == "Radiation") |
      (cause_topic == "Radiation" & effect_topic == "Health Issues") |
      (cause_topic == "Natural Disaster" & effect_topic == "Nuclear Disaster") |
      (cause_topic == "Tsunami" & effect_topic == "Destruction")
  )%>%
  drop_na()

#compute the FDR statistics
g1$fdr <- p.adjust(g1$p.value, method="BH")
g2$fdr <- p.adjust(g2$p.value, method="BH")
g3$fdr <- p.adjust(g3$p.value, method="BH")
g4$fdr <- p.adjust(g4$p.value, method="BH")

#View the tables. These are the final tables in the Sup Info
View(g1)
View(g2)
View(g3)
View(g4)



#BINOMIAL TEST
#get positive and negative counts
#oringinal does it by cause_cluster, effect_cluster; updated (remaped) does on _topic

shift_df2 <- shift_df %>%
  group_by(Structure, Content, cause_topic, effect_topic) %>%
  summarise(
    unique_positive_count = n_distinct(random.ID[difference > 0]),   # Count unique IDs with positive difference
    unique_negative_count = n_distinct(random.ID[difference < 0]),   # Count unique IDs with negative difference
    .groups = 'drop')


shift_df2 %>%
  group_by(Structure, Content) %>%
  summarize(
    total_unique_positive_count = sum(unique_positive_count, na.rm = TRUE),
    total_unique_negative_count = sum(unique_negative_count, na.rm = TRUE),
    .groups = 'drop'
  )


shift_tests <- shift_df2 %>%
  rowwise() %>%
  mutate(
    total_count = unique_positive_count + unique_negative_count,
    binom_test = ifelse(total_count > 0 && unique_positive_count >= 0, 
                        list(binom.test(unique_positive_count, 
                                        total_count, 
                                        p = 0.5, 
                                        alternative = "two.sided", 
                                        conf.level = 0.95)), NA) 
  ) %>%
  ungroup() %>%
  mutate(
    p_value = sapply(binom_test, function(x) if (!is.null(x)) x$p.value else NA),  # Extract p-value
    estimate = sapply(binom_test, function(x) if (!is.null(x)) x$estimate else NA),  # Extract estimate
    conf_int_low = sapply(binom_test, function(x) if (!is.null(x)) x$conf.int[1] else NA),  # Lower bound of confidence interval
    conf_int_high = sapply(binom_test, function(x) if (!is.null(x)) x$conf.int[2] else NA)  # Upper bound of confidence interval
  ) %>% #swap topic and cluster
  select(Structure, Content, cause_topic, effect_topic, unique_positive_count, unique_negative_count, 
         p_value, estimate, conf_int_low, conf_int_high)

shift_tests %>%
  group_by(Structure, Content) %>%
  summarize(
    total_unique_positive_count = sum(unique_positive_count, na.rm = FALSE),
    total_unique_negative_count = sum(unique_negative_count, na.rm = FALSE),
    .groups = 'drop'
  )


sig_shift_tests <- shift_tests %>%
  filter(p_value < .05)

View(sig_shift_tests)
View(shift_tests)


###
#This code will provide additional information on the content of the causal topics. 
###

#Extracting keywords for the topics
#function to create topic keywords
#for each cause topic and effect topic, 

library(grid) 
topic_labels2 <- c(
  '-1' = "No Cluster",
  '0' = "Nuclear Disaster",
  '1' = "Earthquake",
  '2' = "Energy Shortage",
  '3' = "Radiation",
  '4' = "Disaster",
  '5' = "Effects",
  '6' = "Tsunami",
  '7' = "Damage",
  '8' = "Issues",
  '9' = "Change (Loss)",
  '10' = "Health Issues",
  '11' = "Waves",
  '12' = "Terrible Event",
  '13' = "Destruction",
  '14' = "Displacement",
  '15' = "(Tohoku) Earthquake",
  '16' = "Incident",
  '17' = "Natural Disaster",
  '18' = "Tsunami (mispelt)",
  '19' = "Cancer"
)


cause_cluster_counts <- claims_full %>%
  count(cause_cluster) %>%
  rename(ID = cause_cluster, cause_counts = n)

effect_cluster_counts <- claims_full %>%
  count(effect_cluster) %>%
  rename(ID = effect_cluster, effect_counts = n)

merged_counts <- full_join(cause_cluster_counts, 
                           effect_cluster_counts, by = "ID")

# Make cause_counts negative and retain effect_counts as is
merged_counts <- merged_counts %>%
  mutate(cause_counts = -cause_counts) %>%
  mutate(topic_label = topic_labels2[as.character(ID)])


ggplot(merged_counts, aes(x = ID)) +
  geom_bar(aes(y = cause_counts), stat = "identity", fill = "#3A85A8", alpha = 0.8) +
  geom_bar(aes(y = effect_counts), stat = "identity", fill = "#BA6C9E", alpha = 0.8) +
  geom_hline(yintercept = 0, color = "gray30", linetype = "solid", size = 0.8, alpha = 0.7) +
  scale_x_continuous(breaks = seq(min(merged_counts$ID), max(merged_counts$ID), by = 1), 
                     labels = topic_labels2, expand = c(0, 0)) +
  scale_y_continuous(limits = c(-max(abs(merged_counts$cause_counts)), max(abs(merged_counts$cause_counts))),
                     labels = abs, expand = c(0.02, 0)) +
  labs(
    x = "Causal Topic\n(Unsupervised)", 
    y = "Number of Documents\n \u2190 As a Cause | As an Effect \u2192"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray85", size = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.8, "lines"),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  coord_flip()


#To get a sense of the types of entities in each of the cluster, the following two data frames outputs the most frequent entities
#These top entities supply the information in the final topics table in the Sup Info. 
top_causes <- claims_full %>%
  group_by(cause_cluster, cause) %>%
  summarise(frequency = n()) %>%
  arrange(cause_cluster, desc(frequency)) %>%
  group_by(cause_cluster) %>%
  slice_max(order_by = frequency, n = 5) %>%
  ungroup()

View(top_causes)

top_effects <- claims_full %>%
  group_by(effect_cluster, effect) %>%
  summarise(frequency = n()) %>%
  arrange(effect_cluster, desc(frequency)) %>%
  group_by(effect_cluster) %>%
  slice_max(order_by = frequency, n = 5) %>%
  ungroup()

View(top_effects)


###
#In the supplemental materials, we compute shifts in the amount of causal claims in each condition. We model the full distribution as a zero inflated Guassian. Below is the code for that addition.
###


#https://www.andrewheiss.com/blog/2022/05/09/hurdle-lognormal-gaussian-brms/#normally-distributed-outcomes-with-zeros
hurdle_gaussian <- 
  # Create a custom family that is logit if y = 0, normal/gaussian if not
  custom_family("hurdle_gaussian", 
                dpars = c("mu", "sigma", "hu"),
                links = c("identity", "log", "logit"),
                lb = c(NA, 0, NA),
                type = "real")

# Stan code to define hurdle gaussian, regress out the 0s using logistic regression, then fit a gaussian, estimate mean and variance
stan_funs <- "
  real hurdle_gaussian_lpdf(real y, real mu, real sigma, real hu) { 
    if (y == 0) { 
      return bernoulli_lpmf(1 | hu); 
    } else { 
      return bernoulli_lpmf(0 | hu) +  
             normal_lpdf(y | mu, sigma); 
    } 
  }
"

# Prepare Stan code for use in brm()
stanvars <- stanvar(scode = stan_funs, block = "functions")

posterior_predict_hurdle_gaussian <- function(i, prep, ...) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  sigma <- brms::get_dpar(prep, "sigma", i = i)
  theta <- brms::get_dpar(prep, "hu", i = i)
  
  hu <- runif(prep$ndraws, 0, 1)
  ifelse(hu < theta, 0, rnorm(prep$ndraws, mu,sigma))
}

posterior_epred_hurdle_gaussian <- function(prep) {
  with(prep$dpars, mu * (1 - hu))
}

dif_model <- brm(
  formula = bf(difference ~ Content*Structure),
  data = dif_df,
  file = 'models/claim_counts',
  family = hurdle_gaussian, 
  stanvars = stanvars,  
  control = list(adapt_delta = 0.95)
)

summary(dif_model)

