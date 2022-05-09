# Load packages
library(bayesrules)
library(tidyverse)
library(bayesplot)
library(rstanarm)
library(tidybayes)
library(broom.mixed)
library(janitor)

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






