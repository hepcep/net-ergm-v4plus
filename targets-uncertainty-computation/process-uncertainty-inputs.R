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
#    https://anl.box.com/s/k12s6zzdirsoxmuffezy0f507o42jhji
#
# 3. Monte Carlo simulation is used to propagate this uncertainty:
#    - Parameters with known bounds are sampled repeatedly (e.g., 10,000 times)
#    - Each draw is used to compute a full set of derived target statistics
#    - Resulting distributions are summarized with means and 95% confidence intervals
#
# 4. These confidence intervals are overlaid on the ERGM violin plots,
#    which already display the distribution of simulated values for each parameter.
#    This allows a direct visual comparison of how well the ERGM simulation
#    aligns with the uncertainty-informed target expectations.
#


## ----Activate Environment----
rm(list=ls())
library(renv)
renv::activate()


## ---- Libraries ----

library(network)
library(ergm)
library(dplyr)
library(ergm.userterms)
library(here)


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

## ---- Load Ranges for Parameters Other than Degree Distributions ----


