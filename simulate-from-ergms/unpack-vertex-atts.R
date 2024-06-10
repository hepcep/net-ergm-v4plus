# Unpack vertex attributes (example)

rm(list=ls())

# Initialize renv---
renv::restore()


# Libraries ----------

library(here)
library(network)


# Read data ----------

vertex.att.all <- readRDS(file = here("simulate-from-ergms", "out","vertex_att_all_july202023.RDS"))
pwid_w_vertex_names <- readRDS(file=here("simulate-from-ergms", "out", "pwid_w_vertex_names_july2023.RDS"))

# Data description ----------

# vertex.att.all

## vertex.att.all object is a nested list of 100 elements, 
## Each of the 100 lists is composed of a list of 19 elements, corresponding to the attributes below:

# [1] "age"                       "age_lb"                   
# [3] "age_started"               "age_ub"                   
# [5] "agecat"                    "daily_injection_intensity"
# [7] "fraction_recept_sharing"   "hcv_status"               
# [9] "lat"                       "lon"                      
# [11] "na"                        "race"                     
# [13] "race.num"                  "sex"                      
# [15] "syringe_source"            "vertex.names"             
# [17] "young"                     "Zip"                      
# [19] "zipcode"                  

 
# pwid_w_vertex_names

# pwid_w_vertex_names is a dataframe consisting of 31,999 rows and 16 cols
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
  