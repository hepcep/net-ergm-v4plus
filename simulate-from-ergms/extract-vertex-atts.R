#NB: CODE DOES NOT EXECUTE IN RSTUDIO ON OOD (BROWN). COPYING AND PASTING INTO AN INTERACTIVE TERMINAL WORKS. 

# Extract vertex attributes from simulated network 

rm(list=ls())

# Initialize renv---
renv::restore()


# Libraries ----------

library(here)
library(network)

# Load data ----------

load(here("simulate-from-ergms", "out", "on-revamped-oscar-non-randomized-odeg-0-only.RData"))
data <- read.csv(here("data", "synthpop-2023-01-04 21_10_46.csv")) #updated july 20 2023


# Check network list ---------

class(sim_results); length(sim_results)
net1 <- sim_results[[1]]
net1


# Extract the first vertex attribute list from all networks ---------

vertex.att.list <- list.vertex.attributes(net1)
get.vertex.attribute(net1, vertex.att.list[1])
vertex.att.1 <- 
  lapply(sim_results, 
       function(x) get.vertex.attribute(x, vertex.att.list[1])
)
class(vertex.att.1); length(vertex.att.1)
vertex.att.1[1]


# Extract all vertex attribute lists from all networks ---------

vertex.att.all <- vector(mode = "list", length = length(sim_results))  

for(j in 1:length(sim_results)){
  for (i in 1:length(vertex.att.list)){
    vertex.att.all[[j]][[i]] <- get.vertex.attribute(sim_results[[j]],
                                                   vertex.att.list[i]
                                                   )
  }
}

for (i in 1:length(vertex.att.all)){
  # assign attribute names in elements corresponding to the 100 networks
  names(vertex.att.all[[i]]) <- vertex.att.list
}

# Compare agent ordering above to network object ---------

if (nrow(data) == 32001){
  data <- data[-32001,] #the dataset consists of 32001 agents but the networks only contain 32K nodes
  # if statement is to avoid miultiple deletions if this code is run more than once
}
dim(data)

identical(sim_results[[1]] %v% "lat", data$lat)
identical(sim_results[[1]] %v% "lon", data$lon)
## ordering between the data set and the network matches


# Extract vertex names and add to pwid_with_lat_lon.csv datset ---------

vertex.names <- sim_results[[1]] %v% "vertex.names" 
pwid_w_vertex_names <- cbind(vertex.names,
                             data)

# Save data

saveRDS(vertex.att.all, file = here("simulate-from-ergms", "out","vertex_att_all_july202023.RDS"))
saveRDS(pwid_w_vertex_names, file=here("simulate-from-ergms", "out", "pwid_w_vertex_names_july2023.RDS"))