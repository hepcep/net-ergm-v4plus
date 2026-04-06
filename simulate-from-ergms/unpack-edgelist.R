#NB: CODE DOES NOT EXECUTE IN RSTUDIO ON OOD (BROWN). COPYING AND PASTING INTO AN INTERACTIVE TERMINAL WORKS.

# Extract (a single) edgelist from the simulated networks
# Example for Eric Tatara, HepCEP4Py

rm(list=ls())

# Libraries ----------

library(here)
library(network)
library(qs)

# Load data ----------
run_label <- "new-mixing-data-w-dnf"

sim_results <- qread(here("simulate-from-ergms", "out",
                          paste0(run_label, "_sim_results_100.qs")))

class(sim_results[[1]])
list.vertex.attributes(sim_results[[1]])
el <- as.edgelist(sim_results[[1]])
dim(el)
