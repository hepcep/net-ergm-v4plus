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

#load(file=here("fit-ergms", "out", "updated-with-oct12-2024-synthpop-ergmv4-6-all-plosone-terms.RData"))

#load(file=here("fit-ergms", "out", "updated-with-oct12-2024-synthpop-ergmv4-6-all-plosone-terms-increase-mcmc.RData"))

#load(file=here("fit-ergms", "out", "updated-with-oct12-2024-synthpop-ergmv4-6-all-plosone-terms-increase-mcmc-5e5.RData"))

load(file=here("fit-ergms", "out", "updated-with-oct12-2024-synthpop-ergmv4-6-all-plosone-terms-increase-mcmc-1e6.RData"))

# Investigate MCMC diagnositics ----------

mcmc_diag <- mcmc.diagnostics(fit.metadata.mixing)

pdf(file=here("fit-ergms", "out", "mcmc-diags-all-plosone-terms-mcmc-sampsize-int-1e6.pdf"))
plot((fit.metadata.mixing$sample))
dev.off()

gof_results <- gof(fit.metadata.mixing, GOF=~model)
print(gof_results)
