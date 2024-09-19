#NB: CODE DOES NOT EXECUTE IN RSTUDIO ON OOD (BROWN). COPYING AND PASTING INTO AN INTERACTIVE TERMINAL WORKS. 

# Extract (a single) edgelist from the simulated networks 

rm(list=ls())

# Libraries ----------

library(here)
library(network)

load(here("simulate-from-ergms", "out", "simulated-updated-with-oct12-2024-synthpop-ergmv4-6-all-plos1-mcmc-int1e6-samp1e6-hotelling.RData"))

class(sim_results[[1]])
list.vertex.attributes(sim_results[[1]])
el <- as.edgelist(sim_results[[1]])
dim(el)
