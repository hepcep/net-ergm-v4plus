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

load(file=here("fit-ergms", "out", "updated-with-oct12-2024-synthpop-ergmv4-6-all-plos1-mcmc-int1e6-samp1e6-hotelling.RData"))

# Investigate MCMC diagnositics ----------

mcmc_diag <- mcmc.diagnostics(fit.metadata.mixing)

# pdf(file=here("fit-ergms", "out", "updated-with-oct12-2024-synthpop-ergmv4-6-all-plosone-terms-increase-mcmc-samplesize-1e10-mcmcint-1e06.pdf"))
# plot((fit.metadata.mixing$sample))
# dev.off()

gof_results <- gof(fit.metadata.mixing, GOF=~model)
print(gof_results)
plot(gof_results)

pdf(file=here("fit-ergms", "out", "oct12-2024-int1e6-samp1e6-hotelling_gof_plot.pdf"))
plot(gof_results)
dev.off()

