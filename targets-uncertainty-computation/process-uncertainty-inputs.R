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
#    - Resulting resampled distributions are generated (summary stats can be computed from these samples)
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

# -- gender --
  ## parameters
  edges.male.end <- c(0.54, 0.58, 0.61) #specified in lb, mean, ub
  male.pctmale <- c(0.67, 0.68, 0.69)
  edges.female.end <- c(0.36, 0.40, 0.44)
  female.pct.male <- c(0.57, 0.61, 0.64)
  female.pct.female <- c(0.36, 0.39. 0.43)

  ## generate samples of target parameters


# -- race --
 ## parameters
    pct_to_white <- c(0.27, 0.30, 0.33)
    race.w.w <- c(0.62, 0.74, 0.85)
    race.b.w <- c(0.11, 0.31,	0.5)
    race.h.w <- c(0.18, 0.37, 0.57)
    race.o.w <- c(0.35, 0.54,	0.73)

    pct_to_black <- c(0.38, 0.41,	0.45)
    race.w.b <- c(0.05, 0.11, 0.16)
    race.b.b <- c(0.57, 0.3, 0.84)
    race.h.b <- c(0.05,	0.1, 0.14)
    race.o.b <- c(0.08, 0.23, 0.38)

    pct_to_hispanic <- c(0.18, 0.21,	0.24)
    race.w.h <- c(0.06, 0.13, 0.19)
    race.b.h <- c(0.05, 0.07,	0.09)
    race.h.h <- c(0.33, 0.51,	0.69)
    race.o.h <- c(0.10, 0.16,	0.22)

    pct_to_other <- c(0.03, 0.04,	0.06)
    race.w.o <- c(0.004, 0.02,	0.04)
    race.b.o <-  c(0.03, 0.05,	0.07)
    race.h.o <- c(0.0002, 0.0173, 0.0344)
    race.o.o <- c(0.02, 0.07, 0.11)

  ## generate samples

## -- age --
  ## parameters
     edges.young.end	<- c(0.08, 0.1,	0.12)
     young.pctyoung <- c(0.41, 0.61, 0.79)
     young.pctold	<- c(0.21, 0.39, 0.58)
     
     edges.old.end <- c(0.88, 0.9, 0.92)
     old.pctyoung	<- c(0.13, 0.14,	0.15)
     old.pctold	<- c(0.85, 0.86,	0.86)

  ## generate samples of target parameters