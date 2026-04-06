# Analyze synthetic dataset with 32K nodes


# Fit ERGM with 5 dyadic independent terms

rm(list = ls())

# Label and outputs for this run
run_label <- "new-mixing-data-2-race-degen" # set manually to ensure intentional updates

# Activate R environment ----------

library(renv)
renv::activate()


# Libraries ----------

library(network)
library(ergm)
library(dplyr)
library(ergm.userterms)
library(here)
library(styler)


# Create output directory ---------
out_dir <- here("fit-ergms", "out", run_label)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = FALSE)


# Load data objects ----------

data_objects <- readRDS(here("fit-ergms", "out", "processed_data.rds"))
names(data_objects)

intersect(names(data_objects), ls())
list2env(data_objects, envir = globalenv())
ls()

inedges_target <- indegree_data$in_degree * indegree_data$mean_n
sum(inedges_target)

outedges_target <- outdegree_data$out_degree * outdegree_data$mean_n
sum(outedges_target)

edges_target #target number of total edges

# Degree and dist assignments ---------

## outdegrees
deg.terms <- 0:3
deg.terms.0 <- 0
deg.terms.0_1 <- 0:1
deg.terms.0_2 <- 0:2
deg.terms.013 <- c(0, 1, 3)

## indegrees
indeg.terms.0 <- 0
indeg.terms <- 0:1

## dist
dist_as_mixing_matrix
names(dist_as_mixing_matrix)

dist_pct_to_near_pct_to_far
names(dist_pct_to_near_pct_to_far)

## race
class(target_race_num)
target_race_num <- unname(target_race_num)


# Helper function to only run fits that were not previously saved ----------

load_or_run <- function(name, expr) {
  path <- file.path(out_dir, paste0(name, ".rds"))
  if (file.exists(path)) {
    message("Loading saved ", name)
    readRDS(path)
  } else {
    message("Running and saving ", name)
    obj <- eval(expr)
    saveRDS(obj, path)
    obj
  }
}



# Fit Non-empty net including race term ---------
sex_mixing_terms <- summary(n0 ~ nodemix("sex", levels2 = -1))
print(names(sex_mixing_terms))
sex_mixing_align_order <- c(tgt.male.pctfemale, tgt.female.pctmale, tgt.male.pctmale)
print(sex_mixing_align_order)
sum(sex_mixing_align_order)

age_mixing_terms <- summary(n0 ~ nodemix("young", levels2 = -1))
print(names(age_mixing_terms))
age_mixing_align_order <- c(
  tgt.young.pctold,   # 1 → 0
  tgt.old.pctyoung,   # 0 → 1
  tgt.young.pctyoung  # 1 → 1
)
print(age_mixing_align_order)
sum(age_mixing_align_order)

race_mixing_terms <- summary(n0 ~ nodemix("race.num", levels2 = -1))
print(names(race_mixing_terms))
# race order is already aligned
print(target_race_num)
sum(target_race_num)
edges_target

fit_nonempty_network_w_race <-
    ergm(
      edges_only_net ~
        edges +
        nodemix("race", levels2 = -1),
      target.stats =
        c(
          edges_target,
          target_race_num
        ),
      eval.loglik = FALSE,
      control = control.ergm(
        MCMLE.maxit = 500,
        main.method = c("Stochastic-Approximation"),
        MCMC.interval = 1e6,
        MCMC.samplesize = 1e6,
        MCMLE.termination = "Hotelling",
        MCMC.effectiveSize = NULL,
        SAN = control.san(
          SAN.maxit = 500,
          SAN.nsteps = 1e8
        )
      )
    )

  sim_net_race <- simulate(fit_nonempty_network_w_race, nsim = 1)
  sim_net_race


