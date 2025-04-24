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
#    out-degree ributions specified here:
#    https://github.com/hepcep/net-ergm-v4plus/blob/compute-plot-variance/fit-ergms/process-data.R
#
# 2. The uncertainty in mixing parameters (e.g., gender, age, race) is derived
#    from upper and lower bounds obtained via meta-analysis:
#    https://anl.box.com/s/jg6y221hcbql1sd9kvsxpq0cei03qlr0

#
# 3. Monte Carlo simulation is used to propagate this uncertainty:
#    - Parameters with known bounds are sampled repeatedly (e.g., 10,000 times)
#    - Resulting resampled ributions are generated (summary stats can be computed from these samples)
#
# 4. These confidence intervals are overlaid on the ERGM violin plots,
#    which already display the ribution of simulated values for each parameter.
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
# mean and uncertainty ranges for in- and out-degree ributions
# mean values for all other ERGM parameters

# -- input params --
data_objects <- readRDS(here("fit-ergms", "out", "processed_data.rds"))
names(data_objects)

# -- unpack objects in data_objects --

intersect(names(data_objects), ls())
list2env(data_objects, envir = globalenv())
ls()

## ---- Computed ribution of edges ----

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

  in_n_sim <- vector("list", nsim)
  out_n_sim <- vector("list", nsim)
  edges_sim <- numeric(nsim)

  for (i in seq_len(nsim)) {
    in_n <- runif(nrow(indegree_df), indegree_df$lower_n, indegree_df$upper_n)
    out_n <- runif(nrow(outdegree_df), outdegree_df$lower_n, outdegree_df$upper_n)

    in_edges <- sum(indegree_df$in_degree * in_n)
    out_edges <- sum(outdegree_df$out_degree * out_n)

    in_n_sim[[i]] <- in_n
    out_n_sim[[i]] <- out_n
    edges_sim[i] <- mean(c(in_edges, out_edges))

  }

  return(list(
    in_n_sim = in_n_sim,
    out_n_sim = out_n_sim,
    edges_sim = edges_sim
  ))
}

# -- computation  --
result <- simulate_edge_uncertainty(indegree_data, outdegree_data, seed = 42)
edge_samples <- result$edges_sim
head(edge_samples)
length(edge_samples)
mean(edge_samples)
quantile(edge_samples, c(0.025, 0.975))

## ---- Load Ranges for Parameters Other than Degree ributions ----

# -- gender --
    edges.male.end <- c(0.54, 0.58, 0.61) #specified in lb, mean, ub
    male.pctmale <- c(0.67, 0.68, 0.69)


    edges.female.end <- c(0.36, 0.40, 0.44)
    female.pct.male <- c(0.57, 0.61, 0.64)
    female.pct.female <- c(0.36, 0.39, 0.43)

# -- race --
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

## -- age --
     edges.young.end	<- c(0.08, 0.1,	0.12)
     young.pctyoung <- c(0.41, 0.61, 0.79)
     young.pctold	<- c(0.21, 0.39, 0.58)
     
     edges.old.end <- c(0.88, 0.9, 0.92)
     old.pctyoung	<- c(0.13, 0.14,	0.15)
     old.pctold	<- c(0.85, 0.86,	0.86)

## ---- Compute uncertainty in in- and out-degree ributions 

## --indegrees --
inedges <- result$in_n_sim

in_mat <- do.call(rbind, inedges)
head(in_mat)
dim(in_mat)
colnames(in_mat) <- paste0("in_deg_", 0:(ncol(in_mat)-1))  # assuming the first column is degree 0
head(in_mat)

in_deg_summary <- in_mat[, 1:2]  # columns for in-degree 0 and 1
apply(in_deg_summary, 2, quantile, probs = c(0.025, 0.975))

## -- outdegrees --
outedges <- result$out_n_sim
head(outedges)

out_mat <- do.call(rbind, outedges)
head(out_mat)
dim(out_mat)
colnames(out_mat) <- paste0("out_deg_", 0:(ncol(out_mat)-1))  # assumoutg the first column is degree 0
head(out_mat)

out_deg_summary <- out_mat[, 1:2]  # columns for out-degree 0 and 1
apply(out_deg_summary, 2, quantile, probs = c(0.025, 0.975))

## ---- Compute uncertainty in `` parameters 
target_dist_prop <- dist_nedge_distribution/edges_target 
dist_nedge_samples <- edge_samples %*% t(target_dist_prop)
colnames(dist_nedge_samples) <- paste0("dist", 1:4)

head(dist_nedge_samples)

apply(dist_nedge_samples, 2, quantile, probs = c(0.025, 0.975))


## ---- Resample mixing parameters ----
#' Compute edge counts from mixing proportions
#'
#' @param edges Vector of total edge counts (length nsim)
#' @param start_prop Vector of proportions of edges with a specific "start" attribute (length nsim)
#' @param end_prop Vector of conditional proportions for a specific "end" attribute (length nsim)
#'
#' @return Vector of simulated counts for that cell (length nsim)
compute_mixing_counts <- function(edges, start_prop, end_prop) {
  stopifnot(length(edges) == length(start_prop), length(start_prop) == length(end_prop))
  edges * start_prop * end_prop
}

  ## -- gender --
  # Sample proportions
  edges_sim <- result$edges_sim
  nsim <- length(edges_sim)

  ## from males
  edges_male_end   <- runif(nsim, edges.male.end[1], edges.male.end[3])
  male_pctmale     <- runif(nsim, male.pctmale[1], male.pctmale[3])

  ## from females
  edges_female_end   <- runif(nsim, edges.female.end[1], edges.female.end[3])
  female_pctmale     <- runif(nsim, female.pct.male[1], female.pct.male[3])
  female_pctfemale   <- runif(nsim, female.pct.female[1], female.pct.female[3])

  # Compute mixing counts

  ## from males
  n_male_male <- compute_mixing_counts(edges_sim, edges_male_end, male_pctmale)

  ## from females
  n_female_male   <- compute_mixing_counts(edges_sim, edges_female_end, female_pctmale)
  n_female_female <- compute_mixing_counts(edges_sim, edges_female_end, female_pctfemale)

  # Summarize
  quantile(n_male_male, c(0.025, 0.975))
  quantile(n_female_male, c(0.025, 0.975))
  quantile(n_female_female, c(0.025, 0.975))

  # Organize output
  gender_mixing_df <- data.frame(
  male_male = n_male_male,
  female_male = n_female_male,
  female_female = n_female_female
  )

  gender_mixing_quantiles <- apply(gender_mixing_df, 2, quantile, probs = c(0.025, 0.975))
  print(gender_mixing_quantiles)


  ## -- race --
  # Load total edge samples
  edges_sim <- result$edges_sim
  nsim <- length(edges_sim)

  # Sample pct_to_* values (edges ending in race X)
  pct_to_white   <- runif(nsim, pct_to_white[1], pct_to_white[3])
  pct_to_black   <- runif(nsim, pct_to_black[1], pct_to_black[3])
  pct_to_hisp    <- runif(nsim, pct_to_hispanic[1], pct_to_hispanic[3])
  pct_to_other   <- runif(nsim, pct_to_other[1], pct_to_other[3])

  # Sample conditional race.Y.X proportions (edges from Y to X)
  race_w_w <- runif(nsim, race.w.w[1], race.w.w[3])
  race_b_w <- runif(nsim, race.b.w[1], race.b.w[3])
  race_h_w <- runif(nsim, race.h.w[1], race.h.w[3])
  race_o_w <- runif(nsim, race.o.w[1], race.o.w[3])

  race_w_b <- runif(nsim, race.w.b[1], race.w.b[3])
  race_b_b <- runif(nsim, race.b.b[1], race.b.b[3])
  race_h_b <- runif(nsim, race.h.b[1], race.h.b[3])
  race_o_b <- runif(nsim, race.o.b[1], race.o.b[3])

  race_w_h <- runif(nsim, race.w.h[1], race.w.h[3])
  race_b_h <- runif(nsim, race.b.h[1], race.b.h[3])
  race_h_h <- runif(nsim, race.h.h[1], race.h.h[3])
  race_o_h <- runif(nsim, race.o.h[1], race.o.h[3])

  race_w_o <- runif(nsim, race.w.o[1], race.w.o[3])
  race_b_o <- runif(nsim, race.b.o[1], race.b.o[3])
  race_h_o <- runif(nsim, race.h.o[1], race.h.o[3])
  race_o_o <- runif(nsim, race.o.o[1], race.o.o[3])

  # To white
  n_w_w <- compute_mixing_counts(edges_sim, pct_to_white, race_w_w)
  n_b_w <- compute_mixing_counts(edges_sim, pct_to_white, race_b_w)
  n_h_w <- compute_mixing_counts(edges_sim, pct_to_white, race_h_w)
  n_o_w <- compute_mixing_counts(edges_sim, pct_to_white, race_o_w)

  # To black
  n_w_b <- compute_mixing_counts(edges_sim, pct_to_black, race_w_b)
  n_b_b <- compute_mixing_counts(edges_sim, pct_to_black, race_b_b)
  n_h_b <- compute_mixing_counts(edges_sim, pct_to_black, race_h_b)
  n_o_b <- compute_mixing_counts(edges_sim, pct_to_black, race_o_b)

  # To hispanic
  n_w_h <- compute_mixing_counts(edges_sim, pct_to_hisp, race_w_h)
  n_b_h <- compute_mixing_counts(edges_sim, pct_to_hisp, race_b_h)
  n_h_h <- compute_mixing_counts(edges_sim, pct_to_hisp, race_h_h)
  n_o_h <- compute_mixing_counts(edges_sim, pct_to_hisp, race_o_h)

  # To other
  n_w_o <- compute_mixing_counts(edges_sim, pct_to_other, race_w_o)
  n_b_o <- compute_mixing_counts(edges_sim, pct_to_other, race_b_o)
  n_h_o <- compute_mixing_counts(edges_sim, pct_to_other, race_h_o)
  n_o_o <- compute_mixing_counts(edges_sim, pct_to_other, race_o_o)

  # Organize
  race_mixing_df <- data.frame(
  w_w = n_w_w, b_w = n_b_w, h_w = n_h_w, o_w = n_o_w,
  w_b = n_w_b, b_b = n_b_b, h_b = n_h_b, o_b = n_o_b,
  w_h = n_w_h, b_h = n_b_h, h_h = n_h_h, o_h = n_o_h,
  w_o = n_w_o, b_o = n_b_o, h_o = n_h_o, o_o = n_o_o
  )

  race_mixing_quantiles <- apply(race_mixing_df, 2, quantile, probs = c(0.025, 0.975))
  print(race_mixing_quantiles)




  ## -- age --
  # Sample proportions
  edges_sim       <- result$edges_sim
  nsim <- length(edges_sim)
  
  ## from old
  edges_old_end   <- runif(nsim, edges.old.end[1], edges.old.end[3])
  old_pctold      <- runif(nsim, old.pctold[1], old.pctold[3])
  old_pctyoung    <- runif(nsim, old.pctyoung[1], old.pctyoung[3])

  ## from young
  edges_young_end <- runif(nsim, edges.young.end[1], edges.young.end[3])
  young_pctyoung  <- runif(nsim, young.pctyoung[1], young.pctyoung[3])
  young_pctold    <- runif(nsim, young.pctold[1], young.pctold[3])


  # Compute mixing counts

  ## from old
  n_old_old   <- compute_mixing_counts(edges_sim, edges_old_end, old_pctold)
  n_old_young <- compute_mixing_counts(edges_sim, edges_old_end, old_pctyoung)

  ## from young
  n_young_young <- compute_mixing_counts(edges_sim, edges_young_end, young_pctyoung)
  n_young_old   <- compute_mixing_counts(edges_sim, edges_young_end, young_pctold)

  # Summarize
  ## from old
  quantile(n_old_old, c(0.025, 0.975))
  quantile(n_old_young, c(0.025, 0.975))

  ## from young
  quantile(n_young_young, c(0.025, 0.975))
  quantile(n_young_old, c(0.025, 0.975))

 # Organize Output
 age_mixing_df <- data.frame(
  old_old = n_old_old,
  old_young = n_old_young,
  young_young = n_young_young,
  young_old = n_young_old
)

age_mixing_quantiles <- apply(age_mixing_df, 2, quantile, probs = c(0.025, 0.975))
print(age_mixing_quantiles)


 
