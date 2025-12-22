# Analyze synthetic dataset with 32K nodes


# Fit ERGM with 5 dyadic independent terms

rm(list = ls())

# Label and outputs for this run
run_label <- "edges+dnf" # set manually to ensure intentional updates

# Activate R environment ----------

library(renv)
renv::activate()


# Libraries ----------

library(network)
library(ergm)
library(dplyr)
#library(ergm.userterms)
library(ergm.userterms.hepcep)
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

dist.prop.distribution <- c(15.7, 35.1, 24.1, 22)/100 #old dist terms
dist.nedge.distribution <- edges_target*dist.prop.distribution

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

# Incorporating DNF term ---------

## network object
edges_only_net
list.vertex.attributes(edges_only_net)

## We have "lat, lon", "zipcode" information, 
## but not chicago, non-chicago information

  ## use zipcode
  z <- edges_only_net %v% "zipcode"
  is_chicago <- !is.na(z) & substr(as.character(z), 1, 3) == "606"
  edges_only_net %v% "chicago" <- ifelse(is_chicago, 1L, 2L)
  table(edges_only_net %v% "chicago", exclude=NULL)

  list.vertex.attributes(edges_only_net)
  
  stopifnot(all((edges_only_net %v% "chicago") %in% 1:2)) # test assignment

 ## compute target stats
    ## From prior computation (notion, Jul 21, 2025), we know the following:
    ## Of all  edges:
    prop_2.n = 16.3/100 #i.e., 16.3%, and so on...
    prop_2.f = 14.2/100 
    prop_1.n = 45.7/100
    prop_1.f = 22.9/100

    ## Target number of edges is 22959.93
    tgt_2.n <- edges_target * prop_2.n
    tgt_2.f <- edges_target * prop_2.f
    tgt_1.n <- edges_target * prop_1.n
    tgt_1.f <- edges_target * prop_1.f

    c(tgt_1.n, tgt_1.f, tgt_2.n) #specified target statistics
    tgt_2.f #unspecified

    ## Check term ordering for specifying targets
    summary(edges_only_net ~ 
      dnf(by = "chicago", thresholds = c(2,2))
    )

edges_only_net_w_chicago <- edges_only_net
edges_only_net_w_chicago

list.vertex.attributes(edges_only_net_w_chicago)

## Fit ERGM with DNF
fit_nonempty_network_w_dnf <-
    ergm(
      edges_only_net_w_chicago ~ 
        edges +
        dnf(by="chicago", thresholds=c(2, 2)),
      target.stats =
        c(
          edges_target,
          c(tgt_1.n, tgt_1.f, tgt_2.n)
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
  
  
  

non_empty_net_w_dnf <- simulate(fit_nonempty_network_w_dnf, nsim = 1)


non_empty_net_w_dnf
list.vertex.attributes(non_empty_net_w_dnf)

summary(non_empty_net_w_dnf ~       
  dnf(by = "chicago", thresholds = c(2,2))
    )
# Targets: 
c(tgt_1.n, tgt_1.f, tgt_2.n)