# Export edgelists from all simulated networks for HepCep4Py
# Produces a .qs containing a list of 100 edgelists, matched to the
# vertex export in vertex_att_all_*.RDS / pwid_w_vertex_names_*.RDS.
#
# NB: CODE DOES NOT EXECUTE IN RSTUDIO ON OOD (BROWN).
# COPYING AND PASTING INTO AN INTERACTIVE TERMINAL WORKS.

rm(list = ls())


# Libraries ----------

library(here)
library(network)
library(qs)    # read existing sim_results_100.qs (legacy format)
library(qs2)   # write edges_all_*.qs2 for HepCep4Py


# Config ----------

run_label    <- "new-mixing-data-w-dnf"
export_date  <- format(Sys.Date(), "%Y_%b%d")
export_date  <- tolower(export_date)  # e.g. 2026_may21

out_dir      <- here("simulate-from-ergms", "out")
sim_path     <- file.path(out_dir, paste0(run_label, "_sim_results_100.qs"))
synthpop_csv <- here("data", "synthpop-2023-10-12 12_01_32.csv")
edges_out    <- file.path(out_dir, paste0("edges_all_", export_date, ".qs2"))


# Load data ----------

sim_results <- qread(sim_path)
stopifnot(length(sim_results) == 100)

data <- read.csv(synthpop_csv)
if (nrow(data) == 32001) {
  # synthpop has 32001 rows but networks have 32000 nodes
  data <- data[-32001, ]
}


# Crosswalk: verify vertex ordering matches synthpop CSV ----------
# Same check used in crosswalk-extract-vertex-atts.R so edgelist integer
# indices line up with vertex_att_all_*.RDS and pwid_w_vertex_names_*.RDS.

for (k in seq_along(sim_results)) {
  stopifnot(
    "Vertex ordering mismatch (lat)" = identical(sim_results[[k]] %v% "lat", data$lat),
    "Vertex ordering mismatch (lon)" = identical(sim_results[[k]] %v% "lon", data$lon)
  )
}
message("Crosswalk verified: all ", length(sim_results),
        " networks match synthpop ordering.")


# Extract edgelists ----------

edges_all <- lapply(sim_results, function(net) {
  el <- as.edgelist(net)
  # strip network-package attributes so the saved object is just a matrix
  attr(el, "n")          <- NULL
  attr(el, "vnames")     <- NULL
  attr(el, "directed")   <- NULL
  attr(el, "bipartite")  <- NULL
  attr(el, "loops")      <- NULL
  el
})


# Sanity-check stats ----------

edge_counts <- vapply(edges_all, nrow, integer(1))
message("Edge counts across ", length(edges_all), " networks: ",
        "min=", min(edge_counts),
        " median=", stats::median(edge_counts),
        " max=", max(edge_counts))

# Confirm vertex indices are in-range for every network
n_vertices <- network.size(sim_results[[1]])
for (k in seq_along(edges_all)) {
  stopifnot(
    "Edge index out of range" =
      all(edges_all[[k]] >= 1L & edges_all[[k]] <= n_vertices)
  )
}
message("All edge indices in range [1, ", n_vertices, "].")


# Save ----------

qs_save(edges_all, file = edges_out)
message("Saved: ", edges_out)
