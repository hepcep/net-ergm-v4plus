# Compute demographic proportions with Arindam's synthpop data (2022, August) 


rm(list=ls())

# Libraries ----------

library(dplyr)


# Data ----------

demog_data <- read.csv("data/synthpop-2023-10-12 12_01_32.csv")
glimpse(demog_data)
str(demog_data)

# Make race variables consistent with previous dataset ----------

demog_data <-
  demog_data %>% 
  mutate(race=recode(race, "Wh"="W", "Bl"="B", "Hi"="H", "Ot"="O"))

# Relevel groups to match group assignment ----------

levels_sex <- c("M", "F")
levels_race <- c("W", "B", "H", "O")

demog_data <- 
  demog_data %>%
  mutate(sex = factor(sex, levels = levels_sex)) %>%
  mutate(race = factor(race, levels = levels_race))

  
# Compute proportions ----------

demog_dt_props <- 
  demog_data %>%
  group_by(sex, race, agecat) %>%
  tally() %>%
  mutate(prop=n/nrow(demog_data))

colnames(demog_dt_props)


# Write data ----------

write.csv(demog_dt_props, file = "data/demog_dt_props.csv")                
  