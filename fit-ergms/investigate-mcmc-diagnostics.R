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

new_dataset <- "non-empty-net-all-plos1-mcmc-int1e6-samp1e6-hotelling"
new_rdata_object <- paste0(new_dataset, ".RData")


load(file=here("fit-ergms", "out", new_rdata_object))

# Investigate MCMC diagnositics ----------

pdf(file=here("fit-ergms", "out", paste0("mcmc_plot_", new_dataset, ".pdf")))
mcmc_diag <- mcmc.diagnostics(fit.metadata.mixing)
dev.off()


# pdf(file=here("fit-ergms", "out", "updated-with-oct12-2024-synthpop-ergmv4-6-all-plosone-terms-increase-mcmc-samplesize-1e10-mcmcint-1e06.pdf"))
# plot((fit.metadata.mixing$sample))
# dev.off()

gof_results <- gof(fit.metadata.mixing, GOF=~model)
print(gof_results)
#plot(gof_results)

pdf(file=here("fit-ergms", "out", paste0("gof_plot_", new_dataset, ".pdf")))
plot(gof_results)
dev.off()

