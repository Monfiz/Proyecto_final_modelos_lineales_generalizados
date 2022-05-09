# Load packages
library(bayesrules)
library(tidyverse)
library(bayesplot)
library(rstanarm)
library(tidybayes)
library(broom.mixed)
library(janitor)

# Import, rename, & clean data
data(climbers_sub)
climbers <- climbers_sub %>% 
  select(expedition_id, member_id, success, year, season,
         age, expedition_role, oxygen_used)

climbers %>% 
  tabyl(success)

# Size per expedition
climbers_per_expedition <- climbers %>% 
  group_by(expedition_id) %>% 
  summarize(count = n())

# Number of expeditions
nrow(climbers_per_expedition)

# Calculate the success rate for each exhibition
expedition_success <- climbers %>% 
  group_by(expedition_id) %>% 
  summarize(success_rate = mean(success))

# Plot the success rates across exhibitions
ggplot(expedition_success, aes(x = success_rate)) + 
  geom_histogram(color = "white")

# Calculate the success rate by age and oxygen use
data_by_age_oxygen <- climbers %>% 
  group_by(age, oxygen_used) %>% 
  summarize(success_rate = mean(success))

# Plot this relationship
ggplot(data_by_age_oxygen, aes(x = age, y = success_rate, 
                               color = oxygen_used)) + 
  geom_point()


################################################################################

# Modeling 

climb_model <- stan_glmer(
  success ~ age + oxygen_used + (1 | expedition_id), 
  data = climbers, family = binomial,
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735
)

# Confirm prior specifications
prior_summary(climb_model)

# MCMC diagnostics
trace <- mcmc_trace(climb_model, size = 0.1)
mcmc_dens_overlay(climb_model)
mcmc_acf(climb_model)
neff_ratio(climb_model)
rhat(climb_model)







