# Load and inspect the consolidated node table.
# Example for Eric Tatara, HepCEP4Py.
#
# The node table is a single dataframe whose row order matches the integer
# vertex indices in edges_all_*.qs (i.e., row k corresponds to vertex k in
# every simulated network's edgelist).
#
# NB: CODE DOES NOT EXECUTE IN RSTUDIO ON OOD (BROWN).
# COPYING AND PASTING INTO AN INTERACTIVE TERMINAL WORKS.

rm(list = ls())


# Libraries ----------

library(here)


# Load ----------

pwid_node_table <- readRDS(here("simulate-from-ergms", "out",
                                "pwid_node_table_2026_may21.RDS"))


# Inspect ----------

class(pwid_node_table)
dim(pwid_node_table)
colnames(pwid_node_table)
head(pwid_node_table)
