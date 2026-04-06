# USE THE NEW `dnf` TERM


rm(list = ls())

# Activate R environment ----------

library(renv)
renv::activate()


# Libraries ----------

library(network)
library(ergm)
library(dplyr)
library(ergm.userterms.hepcep)

nw.size <- 3
m = matrix(c(rep(1,nw.size-1),2:nw.size), byrow = FALSE, ncol = 2)
ggg <- as.network(m, matrix.type='edgelist',directed = T)
ggg %v% "lat" <- c(41.9,41.889,41.889)
ggg %v% "lat" <- c(41.9,41.889,41.8)
ggg %v% "lon" <- rep(-87,times = nw.size)
ggg %v% "chicago" <- rep(2,size = nw.size, replace = TRUE)

summary(ggg ~ dnf(by = "chicago", thresholds = c(2,2), base = 2))
summary(ggg ~ dnf(by = "chicago", thresholds = c(2,2), base = 3))
summary(ggg ~ dnf(by = "chicago", thresholds = c(2,2)))
