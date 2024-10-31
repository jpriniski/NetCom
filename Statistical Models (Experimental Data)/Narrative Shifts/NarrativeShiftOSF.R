#Modeling shifts in causal language use 
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

setwd("/Users/hunter/Desktop/Nature Human Behavior/Causal Language/")

all_tweets <- read.csv('all_tweets.csv')
claims <- read.csv('claims.csv')

claims_full <- inner_join(claims, all_tweets, by = c("text" = "Response"), 
                          relationship = "many-to-many")

#write.csv(claims_full, file = "/Users/hunter/Desktop/Nature Human Behavior/Causal Language/claims_full.csv", row.names = FALSE)

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

stats_df <- dif_df %>%
  group_by(Content, Structure) %>%
  summarize(mean_difference = mean(difference, na.rm = TRUE),
            sd_difference = sd(difference, na.rm = TRUE),
            n_difference = n(), 
            se_difference = sd_difference / sqrt(n_difference))

stats_df

#Narrative shift plot in the manuscript
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
  ylim(0, 0.5)

#difference in causal language for each condition
t_test_result <- dif_df %>%
  group_by(Content,Structure) %>%
  summarise(t_test_p_value = t.test(difference)$p.value,
            t_test_statistic = t.test(difference)$statistic, 
            t_test_df = t.test(difference)$parameter)

t_test_result



#binomial test to compute the shift across each of the causal relations identified by the causal language algorithim
claims_full <- claims_full %>%
  unite(Identifier, Structure, Content, Size, random.ID, sep = "_", remove = FALSE)

claim_counts <- claims_full %>%
  group_by(Identifier, Structure, Content, Size, random.ID, Phase, cause_cluster, effect_cluster)%>%
  summarise(count = n(), .groups = 'drop')

claim_counts2 <- claim_counts %>%
  complete(Identifier, Phase, cause_cluster, effect_cluster, 
           fill = list(count = 0))%>%
  fill(Structure, Content, Size, random.ID, .direction = "down")


shift_df <- claim_counts2 %>%
  group_by(Structure, Content, random.ID, cause_cluster, effect_cluster) %>%
  reframe(difference = count[Phase == "post"] - count[Phase == "pre"]) 

#get positive and negative counts
shift_df2 <- shift_df %>%
  group_by(Structure, Content, cause_cluster, effect_cluster) %>%
  summarise(
    unique_positive_count = n_distinct(random.ID[difference > 0]),   # Count unique IDs with positive difference
    unique_negative_count = n_distinct(random.ID[difference < 0]),   # Count unique IDs with negative difference
    .groups = 'drop')

View(shift_df2)


shift_tests <- shift_df2 %>%
  rowwise() %>%
  mutate(
    total_count = unique_positive_count + unique_negative_count,
    binom_test = ifelse(total_count > 0 && unique_positive_count >= 0, 
                        list(binom.test(unique_positive_count, 
                                        total_count, 
                                        p = 0.5, 
                                        alternative = "two.sided", 
                                        conf.level = 0.95)), 
                        NA) 
  ) %>%
  ungroup() %>%
  mutate(
    p_value = sapply(binom_test, function(x) if (!is.null(x)) x$p.value else NA),  # Extract p-value
    estimate = sapply(binom_test, function(x) if (!is.null(x)) x$estimate else NA),  # Extract estimate
    conf_int_low = sapply(binom_test, function(x) if (!is.null(x)) x$conf.int[1] else NA),  # Lower bound of confidence interval
    conf_int_high = sapply(binom_test, function(x) if (!is.null(x)) x$conf.int[2] else NA)  # Upper bound of confidence interval
  ) %>%
  select(Structure, Content, cause_cluster, effect_cluster, unique_positive_count, unique_negative_count, 
         p_value, estimate, conf_int_low, conf_int_high)

sig_shift_tests <- shift_tests %>%
  filter(p_value < .05)

View(sig_shift_tests)




#Extracting keywords for the topics
#function to create topic keywords
#for each cause topic and effect topic, 

cause_cluster_counts <- claims_full %>%
  count(cause_cluster) %>%
  rename(ID = cause_cluster, cause_counts = n)

effect_cluster_counts <- claims_full %>%
  count(effect_cluster) %>%
  rename(ID = effect_cluster, effect_counts = n)

merged_counts <- full_join(cause_cluster_counts, effect_cluster_counts, by = "ID")

merged_counts <- merged_counts %>%
  mutate(effect_counts = -effect_counts)


topic_labels <- c(
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
  '15' = "Earthquake",
  '16' = "Incident",
  '17' = "Natural Disaster",
  '18' = "Tsunami*",
  '19' = "Cancer"
)


merged_counts <- merged_counts %>%
  mutate(effect_counts = -effect_counts) %>%
  mutate(topic_label = topic_labels[as.character(ID)])

ggplot(merged_counts, aes(x = ID)) +
  geom_bar(aes(y = cause_counts), stat = "identity", fill = "#71aab7") +
  geom_bar(aes(y = effect_counts), stat = "identity", fill = "#8a4ba5") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 1.5, alpha = .5) +
  scale_x_continuous(breaks = seq(min(merged_counts$ID), max(merged_counts$ID), by = 1), labels = topic_labels,
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(-max(abs(merged_counts$cause_counts)), max(merged_counts$cause_counts)),
                     labels = abs) +  # Set y-axis limits and labels
  labs(x = "Cluster Topic", y = "Number of Documents\n \u2190 As an Effect | As a Cause \u2192") +
  theme_bw() +
  theme(axis.ticks.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(hjust = 0.4, size = 16),  # Center and size the title
        axis.text = element_text(size = 14),  # Make axis text bigger
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        axis.title = element_text(size = 16),  # Make axis labels bigger
        legend.position = "top",
        legend.title = element_text(size = 15),
        legend.key.size = unit(1, "lines"),  # Adjust the size of the legend key
        legend.text = element_text(size = 13))


#To get a sense of the types of entities in each of the cluster, the following two dataframes outputs the most frequent entities
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













