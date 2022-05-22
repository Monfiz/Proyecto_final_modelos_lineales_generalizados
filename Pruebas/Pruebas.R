# Load packages
library(bayesrules)
library(tidyverse)
library(bayesplot)
library(rstanarm)
library(tidybayes)
library(broom.mixed)
library(janitor)
library(caret)

# Load the Data
datos <- read.csv("~/Documents/Repositories/Proyecto_final_modelos_lineales_generalizados/Datos/healthcare-dataset-stroke-data.csv")
datos2 <- read.csv("~/Documents/Repositories/Proyecto_final_modelos_lineales_generalizados/Datos/Stroke_Data.csv")

# Size per expedition
size_urban_rural <- datos2 %>% 
  group_by(Residence_type, hypertension, gender, heart_disease, ever_married) %>% 
  summarize(count = n())

# Calculate the success rate for each exhibition
stroke_success <- datos2 %>% 
  group_by(Residence_type, hypertension, gender, heart_disease, ever_married) %>% 
  summarize(success_rate = mean(stroke))

# Merge dataframes, so we have one with both
strokes_groups <- merge(size_urban_rural, stroke_success)   

# Plot the success rates across exhibitions
ggplot(strokes_groups, aes(x = success_rate)) + 
  geom_histogram(color = "white", bins = 20)

# Calculate the success rate by age and oxygen use
data_by_age_smoking <- datos2 %>% 
  mutate(age = round(age)) |> 
  group_by(age) %>% 
  summarize(success_rate = mean(stroke))

# Plot this relationship
ggplot(data_by_age_smoking, aes(x = age, y = success_rate)) + 
  geom_point()

# Creating IDs

strokes_groups$ID <- seq.int(nrow(strokes_groups))

strokes2 <- datos2 %>%
  mutate(age = round(age)) |> 
  group_by(Residence_type, hypertension, gender, heart_disease, ever_married) %>%
  mutate(group_id = cur_group_id()) |> 
  mutate(y = stroke)

################################################################################

# Modeling 

climb_model <- stan_glmer(
  y ~ age + bmi + avg_glucose_level +  (1 | group_id), 
  data = strokes2, family = binomial(link = "logit"),
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 2, iter = 500*2, seed = 84735
)

# Confirm prior specifications
prior_summary(climb_model)

# MCMC diagnostics
trace <- mcmc_trace(climb_model, size = 0.1)
trace
mcmc_dens_overlay(climb_model)
mcmc_acf(climb_model)
neff_ratio(climb_model)
rhat(climb_model)

#  

strokes2 %>%
  add_fitted_draws(climb_model, n = 100, re_formula = NA) %>%
  ggplot(aes(x = age, y = stroke, color = hypertension)) +
  geom_line(aes(y = .value, group = paste(hypertension, .draw)), 
            alpha = 0.1) + 
  labs(y = "probability of success")

# 

plot(climb_model, prob = 0.5, pars = "beta")

# 

plot(climb_model, plotfun = "areas", prob = 0.95,
     pars = c("(Intercept)"))

plot(climb_model, plotfun = "areas", prob = 0.95,
     pars = c("beta"))

# 

pp_check(climb_model, plotfun = "error_binned")

# 

posterior_vs_prior(climb_model)

# 

pp_check(climb_model, seed = 123)

#

pp_check(climb_model, plotfun = "scatter_avg")

# 

pp_check(climb_model, plotfun = "bars", nreps = 500, prob = 0.5)

# 

pp_check(climb_model, plotfun = "ppc_scatter_avg_grouped", group = "group_id")

# 

plot(climb_model, regex_pars = "^[b]")

# 

classification_summary(data = strokes2, model = climb_model, cutoff = 0.2)





#############

predictions <- posterior_predict(climb_model, newdata = strokes2, type = "response")

preds <- apply(predictions,2, mean)

confusionMatrix(as.factor(strokes2$stroke), as.factor(ifelse(preds > 0.2, 1, 0)))


climb_model2 <- stan_glmer(
  y ~ age + bmi + avg_glucose_level +  (1 | group_id), 
  data = strokes2, family = binomial(link = "cloglog"),
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 2, iter = 500*2, seed = 84735
)

l1 <- loo(climb_model)
l2 <- loo(climb_model2)

loo_compare(l1, l2)








climb_model2 <- stan_glmer(
  stroke ~ age + bmi + avg_glucose_level + Residence_type, ever_married + hypertension + (1 | heart_disease), 
  data = strokes2, family = binomial(link = "logit"),
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 2, iter = 500*2, seed = 84735
)
