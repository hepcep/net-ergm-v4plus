# compute-layout.R

library(here)
library(igraph)
library(intergraph)
library(jsonlite)
library(qs)
library(dplyr)

# Load network object
layout_obj <- qs::qread(here("viz-simulated-nets", "out", "network_data_for_layout.qs"))
net1 <- layout_obj$net1
node_ids <- layout_obj$node_ids
sex_vals <- layout_obj$sex_vals
edge_list <- layout_obj$edge_list

# Convert to igraph
g_igraph <- intergraph::asIgraph(net1)

# Compute force-directed layout
coords <- layout_with_fr(g_igraph)

# Normalize to [-1, 1]
coords <- scale(coords, center = TRUE, scale = apply(abs(coords), 2, max))

# Assemble layout data frame
layout_df <- data.frame(
  id = node_ids,
  x = coords[, 1],
  y = coords[, 2],
  stringsAsFactors = FALSE
)

# Assign sex-based colors
sex_colors <- case_when(
  sex_vals == "M" ~ "#1f77b4",  # blue
  sex_vals == "F" ~ "#d62728",  # red
  TRUE ~ "#aaaaaa"
)

# Create full nodes_df and join layout coords
nodes_df <- data.frame(
  id = node_ids,
  label = node_ids,
  size = 1,
  sex = sex_vals,
  color = sex_colors,
  stringsAsFactors = FALSE
) %>%
  left_join(layout_df, by = "id")

# Format edges
edges_df <- data.frame(
  id = paste0("e", seq_len(nrow(edge_list))),
  source = as.character(edge_list[, 1]),
  target = as.character(edge_list[, 2]),
  stringsAsFactors = FALSE
)

# Create full sigma.js JSON object
sigma_json <- list(nodes = nodes_df, edges = edges_df)

# Write final output
write_json(sigma_json, path = here("viz-simulated-nets", "out", "network_sigma-HEPCEP-force-directed.json"), pretty = TRUE)


