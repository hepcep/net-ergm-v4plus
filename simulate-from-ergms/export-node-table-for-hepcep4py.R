# Verify vertex-attribute invariance across simulated networks AND export
# a single 32,000-row node table for HepCep4Py.
#
# Goal: produce one canonical node-level dataset whose contents *match*
# the synthpop CSV initially read in process-data.R, augmented with the
# columns added during network construction (young, race.num, chicago)
# and the network-package fields (vertex.names, na).
#
# Verification done before exporting:
#   1. Every vertex attribute is identical across all 100 simulated networks.
#      (Required to legitimately collapse 100 attribute lists into one table.)
#   2. Every synthpop CSV column matches the same-named network vertex
#      attribute bitwise (identical()) -- the strong guarantee that the
#      exported table matches the data we initially read.
#
# NB: CODE DOES NOT EXECUTE IN RSTUDIO ON OOD (BROWN).
# COPYING AND PASTING INTO AN INTERACTIVE TERMINAL WORKS.

rm(list = ls())


# Libraries ----------

library(here)
library(network)
library(qs)


# Config ----------

run_label    <- "new-mixing-data-w-dnf"
export_date  <- tolower(format(Sys.Date(), "%Y_%b%d"))  # e.g. 2026_may21

out_dir      <- here("simulate-from-ergms", "out")
sim_path     <- file.path(out_dir, paste0(run_label, "_sim_results_100.qs"))
synthpop_csv <- here("data", "synthpop-2023-10-12 12_01_32.csv")
node_out     <- file.path(out_dir, paste0("pwid_node_table_", export_date, ".RDS"))


# Load ----------

sim_results <- qread(sim_path)
stopifnot(length(sim_results) == 100)

data <- read.csv(synthpop_csv, header = TRUE)
if (nrow(data) == 32001) {
  # synthpop has 32,001 rows; networks only contain 32,000 nodes
  data <- data[-32001, ]
}
stopifnot(nrow(data) == 32000)

ref_net <- sim_results[[1]]
stopifnot(network.size(ref_net) == 32000)


# Step 1: Verify all vertex attributes invariant across all 100 networks ----------

attr_names <- list.vertex.attributes(ref_net)
message("Checking invariance of ", length(attr_names),
        " attributes across ", length(sim_results), " networks: ",
        paste(attr_names, collapse = ", "))

results <- vapply(attr_names, function(a) {
  ref_vals <- get.vertex.attribute(ref_net, a)
  all(vapply(sim_results[-1], function(net) {
    identical(get.vertex.attribute(net, a), ref_vals)
  }, logical(1)))
}, logical(1))
names(results) <- attr_names

print(results)

varying <- names(results)[!results]
if (length(varying) > 0) {
  stop("Vertex attributes vary across simulated networks: ",
       paste(varying, collapse = ", "),
       ". Cannot legitimately collapse 100 attribute lists into one table.")
}
message("All ", length(attr_names),
        " vertex attributes invariant across all 100 networks.")


# Step 2: Verify synthpop columns round-trip against the network ----------
# Strong guarantee that the exported table matches the data initially read in.

shared <- intersect(colnames(data), attr_names)
for (a in shared) {
  if (!identical(get.vertex.attribute(ref_net, a), data[[a]])) {
    stop("Mismatch between network vertex attribute and synthpop column: ", a)
  }
}
message("All ", length(shared),
        " synthpop columns match network vertex attributes bitwise.")


# Step 3: Build the consolidated node table ----------

derived_attrs <- setdiff(attr_names, colnames(data))
message("Adding ", length(derived_attrs),
        " derived/internal vertex attributes: ",
        paste(derived_attrs, collapse = ", "))

derived_df <- as.data.frame(
  setNames(lapply(derived_attrs, function(a) get.vertex.attribute(ref_net, a)),
           derived_attrs),
  stringsAsFactors = FALSE
)

node_table <- cbind(
  vertex.names = get.vertex.attribute(ref_net, "vertex.names"),
  data,
  derived_df[, setdiff(derived_attrs, "vertex.names"), drop = FALSE]
)


# Sanity checks ----------

stopifnot(
  "row count != 32000"             = nrow(node_table) == 32000,
  "synthpop columns dropped"       = all(colnames(data) %in% colnames(node_table)),
  "no duplicate vertex.names"      = !anyDuplicated(node_table$vertex.names),
  "all network attrs represented"  = all(attr_names %in% colnames(node_table))
)

message("Node table: ", nrow(node_table), " rows x ", ncol(node_table), " cols")
message("Columns: ", paste(colnames(node_table), collapse = ", "))


# Save ----------

saveRDS(node_table, file = node_out)
message("Saved: ", node_out)
