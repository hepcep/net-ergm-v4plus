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


# Component distribution

components <- components(g_igraph)
table(components$csize)

# Identify giant component
giant_component_id <- which.max(components$csize)
is_in_gc <- components$membership == giant_component_id

# Subset node data
nodes_gc <- nodes_df[is_in_gc, ]

# Create lookup set for edge filtering
node_ids_gc <- nodes_gc$id
edges_gc <- edges_df %>%
  filter(source %in% node_ids_gc & target %in% node_ids_gc)

# Assemble new JSON
sigma_json_gc <- list(nodes = nodes_gc, edges = edges_gc)

# Assortativity coefficient
assortativity_nominal(g_igraph, types = as.factor(sex_vals), directed = TRUE)



# Write giant component file
write_json(
  sigma_json_gc,
  path = here("viz-simulated-nets", "out", "network_sigma-HEPCEP-force-directed-giant.json"),
  pretty = TRUE
)
