# Unpack vertex attributes (example)
# Example for Eric Tatara, HepCEP4Py

rm(list=ls())

# Initialize renv---
#renv::restore()


# Libraries ----------

library(here)
library(network)


# Read data ----------

vertex.att.all <- readRDS(file = here("simulate-from-ergms", "out","vertex_att_all_2026_apr06.RDS"))
pwid_w_vertex_names <- readRDS(file=here("simulate-from-ergms", "out", "pwid_w_vertex_names_2026_apr06.RDS"))

# Data description ----------

# vertex.att.all

## vertex.att.all object is a nested list of 100 elements,
## Each of the 100 lists is composed of a list of 20 elements, corresponding to the attributes below:

# [1] "age"                       "age_lb"
# [3] "age_started"               "age_ub"
# [5] "agecat"                    "chicago"
# [7] "daily_injection_intensity" "fraction_recept_sharing"
# [9] "hcv_status"                "lat"
# [11] "lon"                       "na"
# [13] "race"                      "race.num"
# [15] "sex"                       "syringe_source"
# [17] "vertex.names"              "young"
# [19] "Zip"                       "zipcode"


# pwid_w_vertex_names

# pwid_w_vertex_names is a dataframe consisting of 32,002 rows and 16 cols
# the column names correspond to > colnames(pwid_w_vertex_names)
# [1] "vertex.names"              "sex"
# [3] "race"                      "agecat"
# [5] "age_started"               "fraction_recept_sharing"
# [7] "syringe_source"            "daily_injection_intensity"
# [9] "age_lb"                    "age_ub"
# [11] "age"                       "zipcode"
# [13] "Zip"                       "lon"
# [15] "lat"                       "hcv_status"
# >
  