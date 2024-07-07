# Investigate MCMC Diagnostics on ERGM Fit

rm(list=ls())

# Activate R environment ------------------------------

library(renv)
renv::activate()


# Libraries ----------

library(network)
library(ergm)
library(dplyr)
library(ergm.userterms)
library(here)


# Load Data ----------

load(file=here("fit-ergms", "out", "updated-with-oct12-2024-synthpop-ergmv4-6-all-plosone-terms.RData"))


# Investigate MCMC diagnositics ----------

mcmc.diagnositics(fit.metadata.mixing)

pdf(file=here("fit-ergms", "out", "mcmc-diags-all-plosone-terms.pdf"))
plot((fit.metadata.mixing$sample))
dev.off()