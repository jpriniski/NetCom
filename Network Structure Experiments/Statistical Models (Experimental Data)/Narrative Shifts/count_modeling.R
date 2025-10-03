
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

custom_colors <- c(
  "#1a88c2ff",
  "#ef1000ff",
  "#6aebe4ff",
  "#d5f294ff",
  "#ffa57eff"
)

setwd("/Users/hunter/Desktop/Nature Human Behavior/Causal Language/")

all_tweets <- read.csv('all_tweets.csv')
claims <- read.csv('claims.csv')

claims_full <- inner_join(claims, all_tweets, by = c("text" = "Response"), 
                          relationship = "many-to-many")

write.csv(claims_full, file = "/Users/hunter/Desktop/Nature Human Behavior/Causal Language/claims_full.csv", row.names = FALSE)

counts_df <- claims_full %>%
  group_by(Structure, Content, Size, Phase, random.ID) %>%
  count()

missing_ids <- all_tweets %>%
  anti_join(counts_df, by = c("Structure", "Content", "Size", "Phase", "random.ID")) %>%
  select(Structure, Content, Size, Phase, random.ID) %>%
  mutate(n = 0)

# Add these missing rows to counts_df
counts_df <- counts_df %>%
  bind_rows(missing_ids)

dif_df <- counts_df %>%
  group_by(Structure, Content, Size, random.ID) %>%
  reframe(difference = n[Phase == "post"] - n[Phase == "pre"]) 


write.csv(dif_df, file = "/Users/hunter/Desktop/Nature Human Behavior/Causal Language/differences_df.csv", row.names = FALSE)

#dif_df$Structure <- factor(dif_df$Structure)
#dif_df$Content <- factor(dif_df$Content)

dif_df %>%
  summarize(proportion_of_zeros = mean(difference == 0)) 

hist(dif_df$difference)

grouped_data <- dif_df %>%
  group_by(Content, Structure) %>%
  summarize(mean_difference = mean(difference, na.rm = TRUE),
            sd_difference = sd(difference, na.rm = TRUE))


dif_df$Structure <- factor(dif_df$Structure, levels = rev(levels(factor(dif_df$Structure))))
grouped_data$Structure <- factor(grouped_data$Structure, levels = rev(levels(factor(grouped_data$Structure))))

ggplot(dif_df, aes(x = Content, y = difference, color = Structure)) +
  geom_jitter(width = 0.2, height = .2, alpha = 0.2, size = 1) +  
  #geom_bar(data = grouped_data, aes(y = mean_difference), size = 3, shape = 21, fill = "white") +  # Mean points
  geom_line(data = grouped_data, aes(y = mean_difference, group = Structure), size = 1) +  # Line connecting means
  labs(x = "Content",
       y = "Difference in Causal Claims") +
  theme_bw()+
  scale_color_manual(values = c("Spatial" = "#E41A1C", "Homogeneous" = "#377EB8"))+
  theme(
    axis.title = element_text(size = 18, face = "bold"),  # Axis titles
    axis.text = element_text(size = 16),                  # Axis text
    legend.title = element_text(size = 18, face = "bold"),# Legend title
    legend.position = "bottom", # Legend position
    legend.text = element_text(size = 16), # Legend text
    strip.text = element_text(size = 18)   # Facet label text
  ) + ylim(-4,4) 


dif_df %>%
  group_by(Structure, Content)%>%
  var(difference)

#no error bars
ggplot(grouped_data, aes(x = Content, y = mean_difference, fill = Structure)) +
  geom_col(position = 'dodge') +  # Mean points
  #geom_errorbar(data = )
  labs(x = "Content",
       y = "Difference in Causal Claims") +
  theme_bw()+
  scale_fill_manual(values = c("Spatial" = "#E41A1C", "Homogeneous" = "#377EB8"))+
  theme(
    axis.title = element_text(size = 18, face = "bold"),  # Axis titles
    axis.text = element_text(size = 16),                  # Axis text
    legend.title = element_text(size = 18, face = "bold"),# Legend title
    legend.position = "bottom", # Legend position
    legend.text = element_text(size = 16), # Legend text
    strip.text = element_text(size = 18)   # Facet label text
  ) + ylim(0,.5) 



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
  formula = bf(difference ~ Content*Structure + Size),
  data = dif_df,
  family = hurdle_gaussian,  # <--- This is new
  stanvars = stanvars,  # <--- This is new
  control = list(adapt_delta = 0.95)
)

dif_model <- brm(
  formula = bf(difference ~ Content*Structure),
  data = dif_df,
  family = hurdle_gaussian, 
  stanvars = stanvars,  
  control = list(adapt_delta = 0.95)
)

summary(dif_model)

#no size, main effect above 0; then back up with the t-test.





dif_model <- brm(
  formula = bf(difference ~ Content*Structure + Size + (1 | random.ID)),
  data = dif_df,
  family = gaussian(), 
  control = list(adapt_delta = 0.95)
)


conditional_effects(dif_model)
summary(dif_model)



#counts_model_old <- brm(
#  formula = bf(n ~ Structure*Content*Phase + Size + (1 + Phase | random.ID)),
#  data = counts_df,
#  family = hurdle_poisson(),
#  control = list(adapt_delta = 0.95)
#)

#strucutr eand content is between, phase is within
#dependent variable is not just freuqncy, it is the difference of post - pre, and then compute interaction
model2 <- brm(
  formula = bf(Count ~ Structure*Phase (1 |random.ID)), 
  data = data,
  family = hurdle_poisson(),
  control = list(adapt_delta = 0.95)
)

summary(counts_model)
pp_check(counts_model)
conditional_effects(counts_model)

posterior_samples <- as.array(counts_model)


color_scheme_set("brightblue")
plot <- mcmc_areas(posterior_samples,
                   pars = c('b_Intercept','b_StructureSpatial','b_ContentHashtag','b_Phasepost'),
                   prob = .95,
                   area_method = 'equal area')

plot + 
  geom_vline(xintercept = 0, linetype = "dotted", color = "black", size = .5)+
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 18)
  )

