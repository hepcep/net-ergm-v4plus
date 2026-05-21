# Verify that vertex attributes are identical across all 100 simulated networks.
# If true (expected, since ERGM simulation only resamples edges on a fixed
# vertex set with fixed covariates), vertex_att_all_*.RDS can be replaced
# with a single 32,000-row attribute table.
#
# NB: CODE DOES NOT EXECUTE IN RSTUDIO ON OOD (BROWN).
# COPYING AND PASTING INTO AN INTERACTIVE TERMINAL WORKS.

rm(list = ls())


# Libraries ----------

library(here)
library(network)
library(qs)


# Load ----------

run_label   <- "new-mixing-data-w-dnf"
sim_results <- qread(here("simulate-from-ergms", "out",
                          paste0(run_label, "_sim_results_100.qs")))

stopifnot(length(sim_results) == 100)


# Attribute list ----------

attr_names <- list.vertex.attributes(sim_results[[1]])
message("Checking ", length(attr_names), " attributes across ",
        length(sim_results), " networks: ",
        paste(attr_names, collapse = ", "))


# Check invariance ----------
# For each attribute, compare every network's vector against network 1.

ref_net <- sim_results[[1]]

results <- vapply(attr_names, function(a) {
  ref_vals <- get.vertex.attribute(ref_net, a)
  all(vapply(sim_results[-1], function(net) {
    identical(get.vertex.attribute(net, a), ref_vals)
  }, logical(1)))
}, logical(1))

names(results) <- attr_names


# Report ----------

message("\nResults (TRUE = identical across all 100 networks):")
print(results)

invariant   <- names(results)[results]
varying     <- names(results)[!results]

message("\nInvariant attributes (", length(invariant), "): ",
        paste(invariant, collapse = ", "))
if (length(varying) > 0) {
  message("Varying attributes (", length(varying), "): ",
          paste(varying, collapse = ", "))
} else {
  message("All vertex attributes are invariant across simulated networks.")
  message("Conclusion: vertex_att_all_*.RDS is redundant; ",
          "a single 32k-row attribute table is sufficient.")
}
