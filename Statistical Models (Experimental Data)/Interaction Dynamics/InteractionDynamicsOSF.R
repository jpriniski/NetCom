#Data visualization and statistical modeling for interaction data 
#Manuscript: Online network topology
#Author: Hunter Priniski

library(readxl)
require(brms)
require(rstan)
require(Rcpp)
require(scales)
require(gridExtra)
require(tidyverse)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

library(entropy)
require(codyn)
require(tidyverse)
library(stringdist)


setwd("/Users/hunter/Desktop/Nature Human Behavior/Interaction Modeling/")

data <- read_csv('interaction_data_osf.csv')

#Unique experimental runs
#codes for naming experimental runs follow 
# content: (h = hashtag, f = facename)
# size (20, 50, 100)
# structure (s = spatial, h = homogeneous)
# run (1, 2, 3)
unique(data$Identifier)

#Factorize Size 
data$DecisionType <- as.factor(data$DecisionType) 

#Compute coordination
data <- data %>%
  mutate(ssid = paste(Identifier, id, sep = "_"))%>%
  group_by(Identifier, TrialNumber, Group) %>%
  mutate(Coordinate = ifelse(n_distinct(Response) == 1, 1, 0)) %>%
  ungroup()

####
# COHERENCE DYNAMICS
####

####
#Global coherence measure 1: Emergence of a normative (dominant) response
####
data_norm <- data %>%
  group_by(Identifier, Structure, Content, Size, TrialNumber) %>%
  summarise(DomResponse = names(sort(table(Response), decreasing = TRUE))[1],
            DomResponseCount = max(table(Response))) %>%
  mutate(SizeF = factor(Size), 
         DomResponseProp = DomResponseCount / Size) %>%
  ungroup() %>%
  group_by(Identifier) %>%
  mutate(DomResponseCountPrev = lag(DomResponseCount),
         DomResponsePropPrev = lag(DomResponseProp))

#reorder to flip plot levels, swtich to Content as column for model fitting
data_norm$Content <- factor(data_norm$Content, levels = rev(levels(factor(data_norm$Content))))
data_norm$Structure <- factor(data_norm$Structure, levels = rev(levels(factor(data_norm$Structure))))

#Visualize the emergence of dominant responses across conditions
# FIGURE 2 in manuscript
ggplot(data_norm, aes(x = TrialNumber, y = DomResponseProp, color = Structure, group = Identifier)) +
  # geom_hline(yintercept = 0, 
  #                linetype = "dotted", 
  #                color = "black", 
  #                linewidth = 0.9)  +
  geom_line(linewidth = .5, alpha = .85) +
  geom_point(size = 1.25, alpha = .7) +
  scale_color_manual(values = c("Spatial" = "#E41A1C", "Homogeneous" = "#377EB8")) +  # Red and Blue colors
  facet_grid(Content ~ Size, drop = TRUE) +
  labs(x = 'Trial', y = "Normative Response", color = "Structure", shape = 'Group Size') +
  #theme_minimal(base_size = 14) +
  theme_bw()+
  theme(
    axis.title = element_text(size = 16, face = "bold"),  # Axis titles
    axis.text = element_text(size = 14),                  # Axis text
    legend.title = element_text(size = 16, face = "bold"),# Legend title
    legend.position = "bottom", # Legend position
    legend.text = element_text(size = 14), # Legend text
    strip.text = element_text(size = 16)   # Facet label text
  ) 

data_norm$Content <- relevel(factor(data_norm$Content), ref = "Face")
data_norm$Structure <- relevel(factor(data_norm$Structure), ref = "Homogeneous")

#Predict proportion counts using a Beta GLM 
dom_model <- brm(DomResponseProp ~ TrialNumber*Structure*Content + Size,
                  data = data_norm,
                  family = Beta(),
                  file = "models/dom_model",
                  prior = c(
                    set_prior("normal(0, 10)", class = "b"),
                    set_prior("normal(0, 10)", class = "Intercept")
                  ),
                  seed = 420)

#pp_check(dom)
summary(dom_model)
conditional_effects(dom_model)


####
#Global coherence measure 2: Entropy decrease in response distribution
####

compute_entropy <- function(response_vector) {
  prob_dist <- table(response_vector) / length(response_vector)
  entropy_value <- entropy(prob_dist, method = "ML")
  return(entropy_value)
}

#Compute trial level entropy
data_entropy <- data %>%
  group_by(Identifier, TrialNumber) %>%
  mutate(Entropy = compute_entropy(Response))

#Compute entorpy shift
data_entropy <- data_entropy %>%
  group_by(Identifier) %>%
  arrange(TrialNumber)%>%
  mutate(StartingEntropy = first(Entropy),
         EntropyDifference = Entropy - StartingEntropy)%>%
  distinct(Identifier, TrialNumber, .keep_all = TRUE)

data_entropy$Content <- factor(data_entropy$Content, levels = rev(levels(factor(data_entropy$Content))))
data_entropy$Structure <- factor(data_entropy$Structure, levels = rev(levels(factor(data_entropy$Structure))))

ggplot(data_entropy, aes(x = TrialNumber, 
                         y = EntropyDifference, 
                         color = Structure, 
                         #shape = as.factor(Size),
                         group = Identifier)) +
  #geom_hline(yintercept = 0, 
  #           linetype = "dotted", 
  #           color = "black", 
  #           linewidth = 0.9)  +
  geom_line(linewidth = .5, alpha = .85) +
  geom_point(size = 1.25, alpha = .7) +
  scale_color_manual(values = c("Spatial" = "#E41A1C", "Homogeneous" = "#377EB8")) +  # Red and Blue colors
  facet_grid(Content ~ Size, drop=TRUE) +
  labs(x = "Trial", y = "Entropy", color = "Structure", shape = 'Group Size') +
  theme_bw()+
  theme(
    axis.title = element_text(size = 16, face = "bold"),  # Axis titles
    axis.text = element_text(size = 14),                  # Axis text
    legend.title = element_text(size = 16, face = "bold"),# Legend title
    legend.position = "bottom",                              # Legend position
    legend.text = element_text(size = 14),                # Legend text
    strip.text = element_text(size = 16)   # Facet label text
  )

data_entropy$Content <- relevel(factor(data_entropy$Content), ref = "Face")
data_entropy$Structure <- relevel(factor(data_entropy$Structure), ref = "Homogeneous")

entropy_model <- brm(
  formula = EntropyDifference ~ TrialNumber*Structure*Content + Size,
  data = data_entropy,
  family = gaussian(), 
  file = 'models/entropy_model',
  prior = c(
    set_prior("normal(0, 10)", class = "b"),
    set_prior("normal(0, 10)", class = "Intercept"),
    set_prior("cauchy(0, 2)", class = "sigma")
  ),
  seed = 420
)

summary(entropy_model)
conditional_effects(entropy_model)

####
#Local coherence measure 1: Probability of linked participant's coordinating
####

data_coord <- data %>%
  group_by(Identifier, TrialNumber, Structure, Content, Size) %>%
  summarize(GroupCoordRate = mean(Coordinate, na.rm = TRUE))%>%
  ungroup()


data_coord$Content <- factor(data_coord$Content, levels = rev(levels(factor(data_coord$Content))))

#Figure SM 2
ggplot(data_coord, aes(x = TrialNumber, y = GroupCoordRate, color = Structure, group = Identifier)) +
  #geom_hline(yintercept = 0, 
  #           linetype = "dotted", 
  #           color = "black", 
  #           size = 0.9)  +
  geom_line(linewidth = .5, alpha = .85) +
  geom_point(size = 1.25, alpha = .7) +
  scale_color_manual(values = c("Spatial" = "#E41A1C",
                                "Homogeneous" = "#377EB8")) +  # Red and Blue colors, because America
  facet_grid(Content ~ Size) +
  labs(x = "Trial Number", y = "Proportion Coordinating", color = "Structure") +
  theme_bw()+
  theme(
    axis.title = element_text(size = 16, face = "bold"),  # Axis titles
    axis.text = element_text(size = 14),                  # Axis text
    legend.title = element_text(size = 16, face = "bold"),# Legend title
    legend.position = "bottom",                              # Legend position
    legend.text = element_text(size = 14),                # Legend text
    strip.text = element_text(size = 16)   # Facet label text
  )

ggsave("coord_dynamics.pdf", plot = d, width = 8, height = 5, device = 'pdf')


#Prob success, use original dataframe to compute prob of trial-level coordination
coord_model <- brm(
  formula = Coordinate ~ TrialNumber*Content*Structure + Size,
  data = data,
  family = bernoulli("logit"),
  file = "models/coord_model",
  prior = c(
    set_prior("normal(0,5)", class = "b"),
    set_prior("normal(0,10)", class = "Intercept")
  ),
  seed = 420
)

pp_check(coord_model)
summary(coord_model)
conditional_effects(coord_model)

####
#Local coherence measure 2: String similarity (normed Lev. distance) of linked responses 
####

#This was a preregistered predecition, we include in the sup materials of manuscript
data_lev <- data %>%
  mutate(
    levenshtein_distance = stringdist(Response, partner_response, method = "lv"),
    avg_length = (str_length(Response) + str_length(partner_response)) / 2,
    normed_levenshtein_distance = levenshtein_distance / avg_length
  )

data_lev <- data_lev %>%
  group_by(TrialNumber, Structure, Content, Size, Identifier) %>%
  summarize(avg_lev = mean(normed_levenshtein_distance, na.rm = TRUE)) %>%
  ungroup()

# Step 2: Calculate the difference between the current trial and the first trial
data_lev <- data_lev %>%
  group_by(Structure, Content, Size, Identifier) %>%
  arrange(TrialNumber) %>%
  mutate(first_trial_avg_lev = first(avg_lev)) %>%
  mutate(lev_diff = avg_lev - first_trial_avg_lev) %>%
  ungroup()

data_lev$Content <- factor(data_lev$Content, levels = rev(levels(factor(data_lev$Content))))

ggplot(data_lev, aes(x = TrialNumber, 
                     y = avg_lev, 
                     color = Structure,
                     group = Identifier)) +
  #geom_hline(yintercept = 0, 
  #           linetype = "dotted", 
  #           color = "black", 
  #           size = 0.9)  +
  geom_line(linewidth = .5, alpha = .85) +
  geom_point(size = 1.25, alpha = .7) +
  scale_color_manual(values = c("Spatial" = "#E41A1C", "Homogeneous" = "#377EB8")) +  # Red and Blue colors
  facet_grid(Content ~ Size) +
  labs(x = "Trial Number", y = "Average String Distance", color = "Structure") +
  theme(
    axis.title = element_text(size = 16, face = "bold"),  # Axis titles
    axis.text = element_text(size = 14),                  # Axis text
    legend.title = element_text(size = 16, face = "bold"),# Legend title
    legend.position = "bottom",                           # Legend position
    legend.text = element_text(size = 14),                # Legend text
    strip.text = element_text(size = 16)   # Facet label text
  )

data_lev <- data %>%
  mutate(
    levenshtein_distance = stringdist(Response, partner_response, method = "lv"),
    avg_length = (str_length(Response) + str_length(partner_response)) / 2,
    normed_levenshtein_distance = levenshtein_distance / avg_length
  )

lev_model <- brm(
  formula = normed_levenshtein_distance ~ TrialNumber*Structure*Content + Size,
  data = data_lev,
  family = gaussian(), 
  file = "models/lev_model",
  prior = c(
    set_prior("normal(0, 10)", class = "b"),
    set_prior("normal(0, 10)", class = "Intercept"),
    set_prior("cauchy(0, 2)", class = "sigma")
  ),
  seed = 420
)

summary(lev_model)
conditional_effects(lev_model)



####
# SOCIAL LEARNING 
####

####
# Which strategies do participants sample? 
####

#make a dataframe with the decision types counted and normalized, for viz, then model with raw df.
data_dt <- data%>%
  group_by(Identifier, Structure, Content, Size, TrialNumber)%>%
  mutate(NumRespondants = n(), .groups = 'drop')%>%
  group_by(Identifier, Structure, Content, Size, TrialNumber, NumRespondants, DecisionType)%>%
  summarise(DTCounts = n())%>%
  mutate(DTNormed = DTCounts / NumRespondants)

#for plotting, will redefine dataframe below for modeling
data_dt <- data_dt%>%
  group_by(Structure, Content, Size, TrialNumber, DecisionType)%>%
  summarize(DTNormedAvg = mean(DTNormed, na.rm = TRUE))%>%
  ungroup()

data_dt$Structure <- factor(data_dt$Structure, levels = rev(levels(factor(data_dt$Structure))))
data_dt$Content <- factor(data_dt$Content, levels = rev(levels(factor(data_dt$Content))))

ggplot(data_dt, aes(x = TrialNumber, 
                    y = DTNormedAvg, 
                    shape = as.factor(Size),
                    color = DecisionType)) +
  geom_line(linewidth = .75, alpha = .5) +
  geom_point(size = 2, alpha = .5) +
  facet_grid(Content ~ Structure) +
  labs(x = "Trial Number", y = "Proportion of Group", color = "Decision Type", shape = 'Group Size') +
  scale_color_manual(values = c("BN" = "#E41A1C", 
                                "EC" = "#377EB8",
                                "RP" = "#4DAF4A", 
                                "RS" = "#984EA3"),
                     labels = c("BN" = "New", 
                                "EC" = "Earlier Context",
                                'RP' = 'Repeat Partner',
                                "RS" = 'Repeat Self'
                     )) +
  guides(color = guide_legend(nrow = 2, order = 1),
         shape = guide_legend(nrow = 1, order = 2)) +  # Split legend into 2 rows
  theme_bw()+
  theme(
    axis.title = element_text(size = 16, face = "bold"),  # Axis titles
    axis.text = element_text(size = 14),                  # Axis text
    legend.title = element_text(size = 16, face = "bold"),# Legend title
    legend.position = "bottom",         # Legend position
    legend.box = "vertical",
    legend.text = element_text(size = 14),                # Legend text
    strip.text = element_text(size = 16)   # Facet label text
  )

#modeling of dec strategies
data_dt <- data%>%
  group_by(Identifier, Structure, Content, Size, TrialNumber)%>%
  mutate(NumRespondants = n(), .groups = 'drop')%>%
  group_by(Identifier, Structure, Content, Size, TrialNumber, NumRespondants, DecisionType)%>%
  summarise(DTCounts = n())%>%
  mutate(DTNormed = DTCounts / NumRespondants)


#Which decision types are sampled, we do not interact with structure because this was not preregisterd, and we do not see an effect of structure (now predict one) on individual sampling startegies
dt_sampled_model <- brm(
  formula = DecisionType ~ TrialNumber*Content + Structure + Size, 
  data = data,
  family = 'categorical',
  file = "models/dt_sampled_model",
  prior = c(
    set_prior("normal(0,5)", class = "b"),
    set_prior("normal(0,10)", class = "Intercept")
  ),
  seed = 420
)

summary(dt_sampled_model)
conditional_effects(dt_sampled_model, categorical = TRUE)



####
# Which strategies do yield rewards? 
####

data_dt_cord <- data%>%
  group_by(Structure, Content, Size, TrialNumber, DecisionType)%>%
  summarize(DTCoordAvg = mean(Coordinate, na.rm = TRUE))%>%
  ungroup()

data_dt_cord$Content <- factor(data_dt_cord$Content, levels = rev(levels(factor(data_dt_cord$Content))))

ggplot(data_dt_cord, aes(x = TrialNumber, 
                         y = DTCoordAvg, 
                         shape = as.factor(Size),
                         color = DecisionType)) +
  #geom_hline(yintercept = 0, 
  #           linetype = "dotted", 
  #           color = "black", 
  #           size = 0.9)  +
  geom_line(linewidth = .75, alpha = .5) +
  geom_point(size = 2, alpha = .5) +
  #scale_color_manual(values = c("Spatial" = "#E41A1C", "Homogeneous" = "#377EB8")) +  # Red and Blue colors
  facet_grid(Content ~ Structure) +
  labs(x = "Trial Number", y = "Proportion Coordinating", color = "Decision Type", shape = 'Group Size') +
  theme(
    axis.title = element_text(size = 16, face = "bold"),  # Axis titles
    axis.text = element_text(size = 14),                  # Axis text
    legend.title = element_text(size = 16, face = "bold"),# Legend title
    legend.position = "bottom",                           # Legend position
    legend.text = element_text(size = 14),                # Legend text
    strip.text = element_text(size = 16)   # Facet label text
  )

#decision strategies coordination

dt_coord_model <- brm(
  formula = Coordinate ~ DecisionType*Content + Structure + Size, 
  data = data,
  family = bernoulli("logit"),
  file = "models/dt_coord_model",
  prior = c(
    set_prior("normal(0,5)", class = "b"),
    set_prior("normal(0,10)", class = "Intercept")
  ),
  seed = 420
)
summary(dt_coord_model)
conditional_effects(dt_coord_model)

dt_coord_model_plot <- plot(conditional_effects(dt_coord_model), points = FALSE, alpha = 0.25)[[5]] 


dt_coord_model_plot <- dt_coord_model_plot + 
  scale_x_discrete(labels = c("BN" = "New", "EC" = "Earlier Context", "RP" = "Repeat Partner", "RS" = "Repeat Self")) +
  labs(x = "Decision Type", y = "Proportion Coordinating", color = "Content") +
  scale_color_manual(values = c("Face" = "#212922", 
                                "Hashtag" = "#71aab7")) +
  theme_bw()+
  theme(
    axis.title = element_text(size = 16, face = "bold"),  # Axis titles
    axis.text = element_text(size = 14),                  # Axis text
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  # X-axis tick labels
    legend.title = element_text(size = 16, face = "bold"),# Legend title
    legend.position = "bottom",                           # Legend position
    legend.text = element_text(size = 14),                # Legend text
    strip.text = element_text(size = 16)   # Facet label text
  )

dt_coord_model_plot




#Prob success over time
dt_coord_time_model <- brm(
  formula = Coordinate ~ TrialNumber*DecisionType*Content + Structure + Size, 
  data = data_dt_cord,
  family = bernoulli("logit"),
  prior = c(
    set_prior("normal(0,5)", class = "b"),
    set_prior("normal(0,10)", class = "Intercept")
  ),
  seed = 420
)

summary(dt_coord_model)
conditional_effects(dt_coord_model)

###
#Impact of semantic content on coordination
###


data_hash <- data %>%
  filter(ResponseType == 'hashtag')

top_25_hashtags <- data_hash %>%
  count(Response) %>%
  arrange(desc(n)) %>%
  top_n(25, n) %>%
  pull(Response)

data_hash_top <- data_hash %>%
  filter(Response %in% top_25_values)

hash_semcoord_model <- brm(
  formula = Coordinate ~ Response, 
  data = data_hash_top,
  family = bernoulli("logit"),
  prior = c(
    set_prior("normal(0,5)", class = "b"),
    set_prior("normal(0,10)", class = "Intercept")
  ),
  seed  = 420
)

summary(hash_semcoord_model)
conditional_effects(hash_semcoord_model)


response_frequencies <- data_hash %>%
  count(Response, sort = TRUE) %>%
  slice_max(n = 25, order_by = n)  

response_frequencies$Response <- fct_reorder(response_frequencies$Response, desc(response_frequencies$n))


ggplot(response_frequencies, aes(x = Response, y = n)) +
  geom_col() +
  labs(title = "25 Most Frequent Hashtags Across Runs",
       x = "Response",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


top25data_hash <- data_hash %>%
  filter(Response %in% response_frequencies$Response)


P_coord_hashtag <- brm(
  formula = Coordinate ~ Response, 
  data = top25data_hash,
  family = bernoulli("logit"),
  prior = c(
    set_prior("normal(0,5)", class = "b"),
    set_prior("normal(0,10)", class = "Intercept")
  ),
  seed = 420
)

conditional_effects(P_coord_hashtag)

plot_object <- plot(conditional_effects(P_coord_hashtag), points = FALSE)$Response

plot_object2 <- plot_object +
  ggplot2::ylim(0, 0.5) +  # Set the y-axis limits
  theme_bw() +  # Use a theme with a white background
  theme(
    plot.title = element_text(hjust = 0.4, size = 16),  # Center and size the title
    axis.text = element_text(size = 14),  # Make axis text bigger
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    axis.title = element_text(size = 16),  # Make axis labels bigger
    legend.position = "top",
    legend.title = element_text(size = 15),
    legend.key.size = unit(1, "lines"),  # Adjust the size of the legend key
    legend.text = element_text(size = 13)  # Adjust the size of the legend text
  ) +
  scale_x_discrete(limits = c("nuclear", "rememberfukushima", 
                              "nucleardisaster", "tsunami","earthquake",
                              "disaster","setsuden",
                              "nuclearaccident", "naturaldisaster",
                              "energy", "tragedy","radioactive",
                              "cancer","powerplant", "nuclearpower","waves",
                              "saveenergy","radiation","ocean",
                              "2011","electricity","nuclearenergy","crisis", "nuclearpowerplant","thyroidcancer")) + 
  xlab("Hashtag") +  # Custom x-axis label
  ylab("P(Player Coordinates)")  # Custom y-axis label


plot_object2



