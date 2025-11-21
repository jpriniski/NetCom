#Data visualization and statistical modeling of network interaction data 
#Manuscript: Network structure shapes collective narrative dynamics through individual decisions
#Script Author: Hunter Priniski (direct all electronic mail to priniski@ucla.edu)

#NOTE: Prior to running this script, unzip the models folder into this directory. It will save time on the model fitting.

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
library(dplyr)

setwd("/Users/hunter/Desktop/PNAS/Data Analysis/Network Interaction/")

data <- read_csv('all_interaction_data.csv')

View(data)

#Column names and values: 
# id - subject's position within a specific network run. Only informative relative to other participants in that run.
# randomID - a subject's unique ID string. Unique to each participant. 
# responeType - name or hashtag response 
# TrialNumber - Trial Number
# Response - The response of a single participant from a single trial
# Group - Pairwise group on a given interaction trial. If two participants in a given network run have the same group on a given trial, then they interacted on that trial
# Identifier - Name of the network condition that they were in (see above)
# Points - 1 or 0 based on if a pair coordinates responses or not
# prev_response - The participant's previous response
# partner_id - The network-specific id of their partner (also see Group)
# partner_prev_response - The subjects previous partner's response
# partner_response
# BC - B if response is new; C if the response comes from earlier context
# Decision Type - How did the individual sample their response? BN = Brand New; RS = Repeat Self; RP = Repeat Partner; EC = Earlier Context
# Content - Interaction media condition; Face or Hashtag
# Size - Network Size
# Structure - Network Structure
# Run - Specific run (1-3) for each newtork condition. 
# ssid - A concatenation of ID, condition, and run. Also serves as a unique participant signifier.

#Unique experimental runs are saved in the Identifier Column (matches the single run csv files in the Data directory)
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
# COHERENCE DYNAMICS. How groups coordinate responses through networked interaction?
####

####
#Emergence of a normative (dominant, shared) response
####

data_norm <- data %>%
  group_by(Identifier, Structure, Content, Size, TrialNumber) %>%
  summarise(
    DomResponse = names(sort(table(Response), decreasing = TRUE))[1], #this will cut off the top response, different across trials
    DomResponseCount = max(table(Response)),
    NumRespondents = n_distinct(`random ID`)  #Because there is missing responses or empty nodes, we want to scale by the number of active participants on a given trial
  ) %>%
  mutate(
    SizeF = factor(Size),
    DomResponseProp = DomResponseCount / NumRespondents
  ) %>%
  ungroup() %>%
  group_by(Identifier) %>%
  mutate(
    DomResponseCountPrev = lag(DomResponseCount),
    DomResponsePropPrev = lag(DomResponseProp)
  )


#Reorder to flip levels for the Figure. 
#Below, we will switch to content = Face as the reference for more interpretable effect estimates
data_norm$Content <- factor(data_norm$Content, levels = rev(levels(factor(data_norm$Content))))
data_norm$Structure <- factor(data_norm$Structure, levels = rev(levels(factor(data_norm$Structure))))

#Figure 2A: Visualize emergence of dominant responses across conditions
dn <- ggplot(data_norm, aes(x = TrialNumber, y = DomResponseProp, color = Structure, group = Identifier)) +
  geom_line(linewidth = .5, alpha = .85) +
  geom_point(size = 1.25, alpha = .7) +
  scale_color_manual(values = c("Spatial" = "#E41A1C", "Homogeneous" = "#377EB8")) +
  facet_grid(Content ~ Size, drop = TRUE) +
  labs(x = 'Trial', y = "Normative Response", color = "Structure", shape = 'Group Size') +
  theme_bw()+
  theme(
    axis.title = element_text(size = 16, face = "bold"),  
    axis.text = element_text(size = 14),                  
    legend.title = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 14), 
    strip.text = element_text(size = 16)  
  ) 

dn
#ggsave("Norm_Shifts.pdf", plot = dn, width = 6, height = 4, dpi = 900)


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

summary(dom_model)

####
#Coherence measure 2: Entropy decrease in response distribution. 
# We report these estimates in the main text, with the figure placed in Sup. Info.
####

compute_entropy <- function(response_vector) {
  prob_dist <- table(response_vector) / length(response_vector)
  entropy_value <- entropy(prob_dist, method = "ML")
  return(entropy_value)
}

#Compute trial level entropy, grouped by experimental run
data_entropy <- data %>%
  group_by(Identifier, TrialNumber) %>% #Grouping by Identifier and Trial Number looks at each trial's entropy in each run
  mutate(Entropy = compute_entropy(Response))

#Compute entropy shift (current trial entropy - group's starting entropy; normalize shifts in entropy across runs, including network sizes)
data_entropy <- data_entropy %>%
  group_by(Identifier) %>%
  arrange(TrialNumber)%>%
  mutate(StartingEntropy = first(Entropy),
         EntropyDifference = Entropy - StartingEntropy)%>%
  distinct(Identifier, TrialNumber, .keep_all = TRUE)

data_entropy$Content <- factor(data_entropy$Content, levels = rev(levels(factor(data_entropy$Content))))
data_entropy$Structure <- factor(data_entropy$Structure, levels = rev(levels(factor(data_entropy$Structure))))

de <- ggplot(data_entropy, aes(x = TrialNumber, 
                         y = EntropyDifference, 
                         color = Structure, 
                         group = Identifier)) +
  geom_line(linewidth = .5, alpha = .85) +
  geom_point(size = 1.25, alpha = .7) +
  scale_color_manual(values = c("Spatial" = "#E41A1C", "Homogeneous" = "#377EB8")) +  # Red and Blue for America
  facet_grid(Content ~ Size, drop=TRUE) +
  labs(x = "Trial", y = "Entropy Shift", color = "Structure", shape = 'Group Size') +
  theme_bw()+
  theme(
    axis.title = element_text(size = 16, face = "bold"),  # Axis titles
    axis.text = element_text(size = 14),                  # Axis text
    legend.title = element_text(size = 16, face = "bold"),# Legend title
    legend.position = "bottom",                              # Legend position
    legend.text = element_text(size = 14),                # Legend text
    strip.text = element_text(size = 16)   # Facet label text
  )


de
#ggsave("Entropy_Shifts.png", plot = de, width = 6, height = 4, dpi = 300)

#Order for model fitting
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

####
#Local coherence measure: Probability of linked participant's coordinating.
#This analysis is included in the Sup Information.
####

#Compute a group's coordination rate 
data_coord <- data %>%
  group_by(Identifier, TrialNumber, Structure, Content, Size) %>%
  summarize(GroupCoordRate = mean(Coordinate, na.rm = TRUE))%>%
  ungroup()


data_coord$Content <- factor(data_coord$Content, levels = rev(levels(factor(data_coord$Content))))

#Figure of the Coordination rates. Figure not included in manuscript or Sup. Info.
#Coordination rate model is described in the Sup. Info.
ggplot(data_coord, aes(x = TrialNumber, y = GroupCoordRate, color = Structure, group = Identifier)) +
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

#ggsave("Coordination_Dynamics.png", plot = de, width = 6, height = 4, dpi = 300)

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

summary(coord_model)


####
# How do individual decision strategies shape network effects on shared responses?
####

####
# Which strategies do participants sample? 
####

#First for effective visualization, we create a data frame with the decision types counted and normalized across groups (mean rates for each strategy)
#We then model the subject-level decision strategies afterwards.
#Data_dt = decision type dataframe
data_dt <- data%>%
  group_by(Identifier, Structure, Content, Size, TrialNumber)%>%
  mutate(NumRespondants = n(), .groups = 'drop')%>%
  group_by(Identifier, Structure, Content, Size, TrialNumber, NumRespondants, DecisionType)%>%
  summarise(DTCounts = n())%>%
  mutate(DTNormed = DTCounts / NumRespondants)

#for plotting, will redefine data frame below for modeling
data_dt <- data_dt%>%
  group_by(Structure, Content, Size, TrialNumber, DecisionType)%>%
  summarize(DTNormedAvg = mean(DTNormed, na.rm = TRUE))%>%
  ungroup()

data_dt$Structure <- factor(data_dt$Structure, levels = rev(levels(factor(data_dt$Structure))))
data_dt$Content <- factor(data_dt$Content, levels = rev(levels(factor(data_dt$Content))))

# Compute average across Size for each condition
avg_dt <- data_dt %>%
  group_by(TrialNumber, DecisionType, Content, Structure) %>%
  summarize(DTNormedAvg = mean(DTNormedAvg, na.rm = TRUE), .groups = 'drop')


# Plot
dt <- ggplot(data_dt, aes(x = TrialNumber, 
                          y = DTNormedAvg, 
                          shape = as.factor(Size), 
                          color = DecisionType)) +
  # Base lines/points (all conditions)
  geom_line(linewidth = .5, alpha = .85) +
  geom_point(size = 1.25, alpha = .7) +
  
  # Overlay average lines for BN and RS
  geom_line(data = avg_dt %>% filter(DecisionType %in% c("BN", "RS")),
            aes(x = TrialNumber, y = DTNormedAvg, color = DecisionType),
            linewidth = 1, alpha = 0.9, inherit.aes = FALSE) +
  
  facet_grid(Content ~ Structure) +
  labs(x = "Trial Number", y = "Proportion of Group", 
       color = "Decision Type", shape = 'Group Size') +
  scale_color_manual(values = c(
    "EC" = "#bcb5a0",
    "RP" = "#fff1cc", 
    "RS" = "#FDB462", "BN" = "#36454F"
  ),
  labels = c(
    "BN" = "New", 
    "EC" = "Earlier Context",
    "RP" = "Repeat Partner", 
    "RS" = "Repeat Self"
  )) +
  guides(color = guide_legend(nrow = 2, order = 1),
         shape = guide_legend(nrow = 1, order = 2)) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16, face = "bold"),  # Axis titles
    axis.text = element_text(size = 12),                  # Axis text
    legend.title = element_text(size = 14, face = "bold"),# Legend title
    legend.position = "bottom",         # Legend position
    legend.box = "vertical",
    legend.text = element_text(size = 12),                # Legend text
    strip.text = element_text(size = 14)   # Facet label text
  )

dt
ggsave("Decision_Dynamics.pdf", plot = dt, width = 6.67, height = 5.8, dpi = 900)


#Modeling individual level decision strategies
data_dt <- data%>%
  group_by(Identifier, TrialNumber)%>%
  mutate(NumRespondants = n(), .groups = 'drop')%>%
  group_by(Identifier, Structure, Content, Size, TrialNumber, NumRespondants, DecisionType)%>% #group by these variables to easily keep columns for stat analysis
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

summary(dt_sampled_model) #This summary table is presented in Table SM 2 (sup info)


# Also in the Sup Info, we discuss how different decision strategies result in 
#coordination across network conditions. 

data_dt_cord <- data%>%
  group_by(Structure, Content, Size, TrialNumber, DecisionType)%>%
  summarize(DTCoordAvg = mean(Coordinate, na.rm = TRUE))%>%
  ungroup()

data_dt_cord$Content <- factor(data_dt_cord$Content, levels = rev(levels(factor(data_dt_cord$Content))))

#decision strategies coordination. We don't interact with structure.
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


#Not included in manuscript, but you can visulize the conditional effects here:
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
#ggsave("Decision_coordination.png", plot = dt_coord_model_plot, width = 6, height = 6, dpi = 300)


###
#Additional Supplemental Information Figures. 
###


# Prior distributions for face and hashtag content.

#Priors are computed by measuring the distribution of responses on the first trial of each game, aggregated across all of the network runs.
faces_first <- data %>%
  filter(ResponseType == 'name')%>%
  filter(TrialNumber == 1)%>%
  select('Response')

hash_first <- data %>%
  filter(ResponseType == 'hashtag')%>%
  filter(TrialNumber == 1)%>%
  select('Response')
  

top_faces <- faces_first %>%
  count(Response, sort = TRUE) %>%
  slice_max(n, n = 15) %>%
  mutate(Type = "Face Names", Response = Response)

top_hashes <- hash_first %>%
  count(Response, sort = TRUE) %>%
  slice_max(n, n = 15) %>%
  mutate(Type = "Hashtags", Response = Response)

combined_data <- bind_rows(top_faces, top_hashes)

#Figure SM 5 in Sup Info. 
ggplot(combined_data, aes(x = reorder(Response, n), y = n, fill = Type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~ Type, scales = "free_y") +
  labs(x = "Response", y = "Frequency") +
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
        legend.text = element_text(size = 13),
        strip.text = element_text(size = 13))+
  scale_fill_manual(values = c("Face Names" = "#17201b", "Hashtags" = "#71aab7")) +  
  theme(legend.position = "none")



###
#Impact of semantic content on coordination: Hashtag
###

data_hash <- data %>%
  filter(ResponseType == 'hashtag')

top_25_hashtags <- data_hash %>%
  count(Response) %>%
  arrange(desc(n)) %>%
  top_n(25, n) %>%
  pull(Response)

data_hash_top <- data_hash %>%
  filter(Response %in% top_25_hashtags)


#The posteriors don't sample well given the high variability in hashtag responses. 
#These estimates should be viewed more as raw proportion values (number of coordinations/total trials generated for each hashtag )
hash_semcoord_model <- brm(
  formula = Coordinate ~ Response, 
  data = data_hash_top,
  family = bernoulli("logit"),
  file = "models/hash_coord",
  prior = c(
    set_prior("normal(0,5)", class = "b"),
    set_prior("normal(0,10)", class = "Intercept")
  ),
  seed  = 420
)

#summary(hash_semcoord_model)

plot_object_hash <- plot(conditional_effects(hash_semcoord_model), points = FALSE)$Response

plot_object_hash2 <- plot_object_hash +
  ggplot2::ylim(0, 0.7) +  # Set the y-axis limits
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
  xlab("Hashtag") +  
  ylab("P(Player Coordinates)")  +
  coord_flip()

plot_object_hash2

###
#Face coordination rates
###

data_face <- data %>%
  filter(ResponseType == 'name')

top_25_faces <- data_face %>%
  count(Response) %>%
  arrange(desc(n)) %>%
  top_n(25, n) %>%
  pull(Response)

data_face_top <- data_face %>%
  filter(Response %in% top_25_faces)


P_coord_faces <- brm(
  formula = Coordinate ~ Response, 
  data = data_face_top,
  family = bernoulli("logit"),
  file = "models/face_coord",
  prior = c(
    set_prior("normal(0,5)", class = "b"),
    set_prior("normal(0,10)", class = "Intercept")
  ),
  seed = 420
)


plot_object_face <- plot(conditional_effects(P_coord_faces), points = FALSE)$Response


plot_object_face2 <- plot_object_face +
  ggplot2::ylim(0, 0.7) +  # Set the y-axis limits
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
  scale_x_discrete(limits = c( "emily", "maddie",  "taylor",   "samantha", 
                               "emma" ,   "sara" , "amber"  , "amanda"  ,   "jane" ,  "abby",
                               "sarah",    "janet", "judy" , "becky"  ,
                               "amy"  ,    "mary"  ,  
                               "jessica" ,  "anna"  ,  
                               "katie" , "karen"  ,     
                               "sam"  , "kate",   "ashley"  ,  "rachel" ,   "alex" 
  )) + 
  xlab("Name") + 
  ylab("P(Player Coordinates)") +
  coord_flip()


#Plot both on the same Figure. Figure SM 6 in Sup Info. 
grid.arrange(plot_object_face2, plot_object_hash2, ncol = 2)


