#NB: CODE DOES NOT EXECUTE IN RSTUDIO ON OOD (BROWN). COPYING AND PASTING INTO AN INTERACTIVE TERMINAL WORKS. 

# Extract vertex attributes from simulated network 

rm(list=ls())

# Initialize renv---
#renv::restore()


# Libraries ----------


library(here)
library(network)
library(qs)

# Load data ----------
run_label <- "new-mixing-data-w-dnf"

sim_results <- qread(here("simulate-from-ergms", "out",
                          paste0(run_label, "_sim_results_100.qs")))
                          ## swap between _10.qs for quicker check
                          ## and _100.qs for full run

#load(here("simulate-from-ergms", "out", "simulated-updated-with-oct12-2024-synthpop-ergmv4-6-all-plos1-mcmc-int1e6-samp1e6-hotelling.RData"))

data <- read.csv(here("data", "synthpop-2023-10-12 12_01_32.csv")) #updated oct 12 2023


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

# Crosswalk: verify vertex ordering in simulated networks matches synthpop CSV ---------
# The synthpop CSV has no explicit agent ID column. Each agent has a unique
# (lat, lon) pair, so we use these as a fingerprint to confirm that vertex
# ordering in every simulated network is aligned with the CSV row ordering.

n_net <- network.size(sim_results[[1]])
if (nrow(data) > n_net) {
  message("Dropping ", nrow(data) - n_net,
          " extra row(s) from end of CSV to match network size.")
  data <- data[seq_len(n_net), ]
}
stopifnot(nrow(data) == n_net)
dim(data)

for (k in seq_along(sim_results)) {
  stopifnot(
    "Vertex ordering mismatch (lat)" = identical(sim_results[[k]] %v% "lat", data$lat),
    "Vertex ordering mismatch (lon)" = identical(sim_results[[k]] %v% "lon", data$lon)
  )
}
message("Crosswalk verified: all ", length(sim_results), " networks match synthpop ordering.")


# Extract vertex names and add to pwid_with_lat_lon.csv datset ---------

vertex.names <- sim_results[[1]] %v% "vertex.names" 
pwid_w_vertex_names <- cbind(vertex.names,
                             data)

# Save data

saveRDS(vertex.att.all, file = here("simulate-from-ergms", "out","vertex_att_all_2026_apr06.RDS"))
saveRDS(pwid_w_vertex_names, file=here("simulate-from-ergms", "out", "pwid_w_vertex_names_2026_apr06.RDS"))