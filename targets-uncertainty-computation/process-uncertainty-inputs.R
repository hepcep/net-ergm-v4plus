#' ---
#' title: "Computing Uncertainty in Target Stats for ERGM"
#' author: "Aditya Khanna for HepCep Team"
#' date: "`r Sys.Date()`"
#' output: html_document
#' ---

# ----------------------------------------------------------------------
# Strategy for Overlaying Target Parameter Uncertainty on ERGM Plots
# ----------------------------------------------------------------------
# This section outlines the approach used to account for uncertainty
# in the ERGM target statistics and visualize them alongside simulation results.
#
# 1. The uncertainty in the number of edges is derived from the in-degree and
#    out-degree distributions specified here:
#    https://github.com/hepcep/net-ergm-v4plus/blob/compute-plot-variance/fit-ergms/process-data.R
#
# 2. The uncertainty in mixing parameters (e.g., gender, age, race) is derived
#    from upper and lower bounds obtained via meta-analysis:
#    https://anl.box.com/s/jg6y221hcbql1sd9kvsxpq0cei03qlr0


#
# 3. Monte Carlo simulation is used to propagate this uncertainty:
#    - Parameters with known bounds are sampled repeatedly (e.g., 10,000 times)
#    - Resulting resampled distributions are summarized with means and 95% confidence intervals
#
# 4. These confidence intervals are overlaid on the ERGM violin plots,
#    which already display the distribution of simulated values for each parameter.
#    This allows a direct visual comparison of how well the ERGM simulation
#    aligns with the uncertainty in the expected target values.
#


## ----Activate Environment----
rm(list = ls())
library(renv)
renv::activate()


## ---- Libraries ----

library(network)
library(ergm)
library(dplyr)
library(ergm.userterms)
library(here)
library(styler)


## ---- Load Pre-Processed Data ----

# This section loads previously processed input data for ERGM fitting, including
# mean and uncertainty ranges for in- and out-degree distributions
# mean values for all other ERGM parameters

# -- input params --
data_objects <- readRDS(here("fit-ergms", "out", "processed_data.rds"))
names(data_objects)

# -- unpack objects in data_objects --

intersect(names(data_objects), ls())
list2env(data_objects, envir = globalenv())
ls()

## ---- Computed distribution of edges ----

# -- function --

#' Simulate uncertainty in edge count
#'
#' @param indegree_df Data frame with in_degree, lower_n, and upper_n columns.
#' @param outdegree_df Data frame with out_degree, lower_n, and upper_n columns.
#' @param nsim Number of simulations (default = 10000).
#' @param seed Optional random seed.
#' @return Numeric vector of simulated edge counts.

simulate_edge_uncertainty <- function(indegree_df, outdegree_df, nsim = 10000, seed = 1234) {
  if (!is.null(seed)) set.seed(seed)

  # Keep only rows where lower_n <= upper_n and neither is NA
  indegree_df <- indegree_df %>% filter(!is.na(lower_n), !is.na(upper_n), lower_n <= upper_n)
  outdegree_df <- outdegree_df %>% filter(!is.na(lower_n), !is.na(upper_n), lower_n <= upper_n)

  edges_sim <- numeric(nsim)

  for (i in seq_len(nsim)) {
    in_n <- runif(nrow(indegree_df), indegree_df$lower_n, indegree_df$upper_n)
    out_n <- runif(nrow(outdegree_df), outdegree_df$lower_n, outdegree_df$upper_n)

    in_edges <- sum(indegree_df$in_degree * in_n)
    out_edges <- sum(outdegree_df$out_degree * out_n)

    edges_sim[i] <- mean(c(in_edges, out_edges))
  }

  return(edges_sim)
}



# -- computation  --
edge_samples <- simulate_edge_uncertainty(indegree_data, outdegree_data, seed = 42)
head(edge_samples)
length(edge_samples)
mean(edge_samples)
quantile(edge_samples, c(0.025, 0.975))

## ---- Load Ranges for Parameters Other than Degree Distributions ----
