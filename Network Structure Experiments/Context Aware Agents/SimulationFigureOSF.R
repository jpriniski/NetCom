#simulation figure

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

#laptop, needs updating with final run.
setwd("/Users/hunter/Desktop/Nature Human Behavior/Simulations/")

#macmini
setwd("~/Desktop/Nature Human Behavior/Simulations/Entropy Optimization/")

data <- read_csv('EntropyComparisonFig.csv')


df_long <- data %>%
  pivot_longer(cols = `Experimental Homogeneous Hashtag`:`CAA Spatial Face`,  # Select the range of columns
               names_to = "Model Group", 
               values_to = "Entropy") %>%
  separate(`Model Group`, into = c("Model", "Structure", "Content"), sep = " ")
#%>%
#  mutate(Model = if_else(Model == "CABA", "CAA", Model))


mod_rows <- df_long %>%
  filter(Model != 'Experimental')%>%
  mutate(Marking = 'Simulation')

exp1 <- df_long %>%
  filter(Model == 'Experimental')%>%
  mutate(Model = 'Centola')%>%
  mutate(Marking = 'Human')

exp2 <- df_long %>%
  filter(Model == 'Experimental')%>%
  mutate(Model = 'CAA')%>%
  mutate(Marking = 'Human')


df_long <- bind_rows(mod_rows, exp1, exp2)

df_long$Content <- as.factor(df_long$Content)
df_long$Model <- as.factor(df_long$Model)

df_long$Content <- factor(df_long$Content, levels = rev(levels(df_long$Content)))  # Reverse levels for Content
df_long$Model <- factor(df_long$Model, levels = rev(levels(df_long$Model)))  # Reverse levels for Model


d <- ggplot(df_long, aes(x = Trial, y = Entropy, color = Structure, shape = Marking)) +
  geom_line() +
  geom_point() +  # Main points from df_long
  facet_grid(rows = vars(Content), cols = vars(Model)) + 
  labs(x = "Trial", y = "Entropy") +
  theme_bw()+
  guides(color = guide_legend(nrow = 2, order = 2),
         shape = guide_legend(nrow = 2, order = 1)) + 
  scale_color_manual(values = c("Spatial" = "#E41A1C", "Homogeneous" = "#377EB8"))+
  scale_shape_manual(values = c("Simulation" = 24, "Human" = 16)) +
  theme(
    axis.title = element_text(size = 18, face = "bold"),  # Axis titles
    axis.text = element_text(size = 16),                  # Axis text
    legend.title = element_text(size = 18, face = "bold"),# Legend title
    legend.position = "bottom", # Legend position
    legend.text = element_text(size = 16), # Legend text
    strip.text = element_text(size = 18)   # Facet label text
  )


d

ggsave("simulations.pdf", plot = d, width = 6.5, height = 6, device = 'pdf')


