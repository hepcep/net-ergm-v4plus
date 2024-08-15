# Simulate network (n=32K) from ERGM fit 
# parameterized with meta data  


rm(list=ls())

# Initiate environment ------------------------------

library(renv)
renv::activate()

.libPaths()


# Libraries ----------

library(here)


# Data ----------

load(here("simulate-from-ergms", "out", "sim-updated-with-oct12-2024-synthpop-ergmv4-6-all-plos1-mcmc-int1e6-samp1e6-hotelling.RData"))
