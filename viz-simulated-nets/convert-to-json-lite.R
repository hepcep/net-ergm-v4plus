# Convert to jsonlite for plotting with sigma.js

rm(list = ls())


# Initiate environment ------------------------------

library(renv)
renv::activate()

.libPaths()


# Libraries ----------

library(network)
library(ergm)
library(dplyr)
library(ergm.userterms)
library(here)
library(ggplot2)
library(qs)
library(jsonlite)

# Data ----------

run_label <- "stepwise-refactored-checkpointing-data-dated-2025-jan23-redone" # set manually to ensure intentional updates
## should match the object from the ERGM fitting code 

  ## load data
  ## sim_results <- qread(here("simulate-from-ergms", "out", paste0(run_label, "_sim_results_10.qs")))
  sim_results <- qread(here("simulate-from-ergms", "out", paste0(run_label, "_sim_results_100.qs")))


## input params
  data_objects <- readRDS(here("fit-ergms", "out", "processed_data.rds"))
  names(data_objects)

## confirm which run
  run_label

## Unpack objects in data_objects
intersect(names(data_objects), ls())
list2env(data_objects, envir = globalenv())
ls()

# Extract network --------
net1 <- sim_results[[1]]
net1

# Convert to jsonlite format --------

# Step 1: Extract nodes
node_ids <- seq_len(network.size(net1))  # 1 to n
node_attrs <- list.vertex.attributes(net1)

set.seed(123)
n <- length(node_ids)

# Normalized coords
x_vals <- runif(n, -1, 1)
y_vals <- runif(n, -1, 1)
sex_vals <- net1 %v% "sex"

# Assign sex-based color
sex_colors <- case_when(
  sex_vals == "M" ~ "#1f77b4",  # blue
  sex_vals == "F" ~ "#d62728",  # red
  TRUE ~ "#aaaaaa"
)

# Create final node data frame
nodes_df <- data.frame(
  id = as.character(node_ids),
  label = as.character(node_ids),
  x = x_vals,
  y = y_vals,
  size = 1,
  sex = sex_vals,
  color = sex_colors,
  stringsAsFactors = FALSE
)

# Step 2: Extract edges
edge_list <- as.edgelist(net1)

edges_df <- data.frame(
  id = paste0("e", seq_len(nrow(edge_list))),
  source = as.character(edge_list[, 1]),
  target = as.character(edge_list[, 2]),
  stringsAsFactors = FALSE
)

# Step 3: Build JSON structure
sigma_json <- list(nodes = nodes_df, edges = edges_df)

# Step 4: Write JSON to file
write_json(sigma_json, path = here("viz-simulated-nets", "out", "network_sigma-HEPCEP.json"), pretty = TRUE)

