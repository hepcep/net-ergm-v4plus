# Analyze synthetic dataset with 32K nodes


# Fit ERGM with 5 dyadic independent terms

rm(list = ls())

# Label and outputs for this run
run_label <- "new-mixing-data-2" # set manually to ensure intentional updates

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
dist.terms <- 1:3 # fourth is left out

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


race_mixing_terms <- summary(n0 ~ nodemix("race.num"))
print(names(race_mixing_terms))
# race order is already aligned on num
print(target_race_num)
sum(target_race_num)

race_mixing_terms_cat <- summary(n0 ~ nodemix("race"))
print(names(race_mixing_terms))
term_names <- names(race_mixing_terms_cat)
desired_order <- c(
  "mix.race.Wh.Wh", "mix.race.Wh.Bl", "mix.race.Wh.Hi", "mix.race.Wh.Ot",
  "mix.race.Bl.Wh", "mix.race.Bl.Bl", "mix.race.Bl.Hi", "mix.race.Bl.Ot",
  "mix.race.Hi.Wh", "mix.race.Hi.Bl", "mix.race.Hi.Hi", "mix.race.Hi.Ot",
  "mix.race.Ot.Wh", "mix.race.Ot.Bl", "mix.race.Ot.Hi", "mix.race.Ot.Ot"
)

# Reorder
race_mixing_terms_cat_ordered <- race_mixing_terms_cat[desired_order]
race_mixing_terms_cat_ordered


fit_nonempty_network_w_sex <-
  load_or_run("fit_nonempty_network_w_sex", quote(
    ergm(
      edges_only_net ~
        edges +
        nodemix("sex", levels2 = -1),
      target.stats =
        c(
          edges_target,
          sex_mixing_align_order
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
  ))

non_empty_net_w_sex <- load_or_run("non_empty_net_w_sex", quote(
  simulate(fit_nonempty_network_w_sex, nsim = 1)
))
non_empty_net_w_sex

fit_nonempty_network_w_sex_age <-
  load_or_run("fit_nonempty_network_w_sex_age", quote(
    ergm(
      non_empty_net_w_sex ~
        edges +
        nodemix("sex", levels2 = -1)+
        nodemix("young", levels2 = -1),
      target.stats =
        c(
          edges_target,
          sex_mixing_align_order,
          age_mixing_align_order
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
  ))

non_empty_net_w_sex_age <- load_or_run("non_empty_net_w_sex_age", quote(
  simulate(fit_nonempty_network_w_sex_age, nsim = 1)
))
non_empty_net_w_sex_age

race_levels <- setdiff(
  as.list(as.data.frame(t(expand.grid(1:4, 1:4)))),
  list(c(4, 4))
)

fit_nonempty_network_w_race_num <-
  load_or_run("fit_nonempty_network_w_race_num", quote(
    ergm(
      edges_only_net ~
        edges +
        #nodemix("sex", levels2 = -1) +
        #nodemix("young", levels2 = -1) +
        nodemix("race", levels2 = race_levels),
      target.stats =
        c(
          edges_target,
          #sex_mixing_align_order,
          #age_mixing_align_order,
          target_full_race_matrix[1:15]
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
  ))

non_empty_net_w_race_term <- load_or_run("net_nonempty_w_race_term", quote(
  simulate(fit_nonempty_network_w_race_num, nsim = 1)
))
non_empty_net_w_race_term

# fit.stepwise.dist <-
#   load_or_run("fit.stepwise.dist", quote(
#     ergm(
#       non_empty_net_w_race_term ~
#         edges +
#         nodemix("sex", levels2 = -1) +
#         nodemix("young", levels2 = -1) +
#         nodemix("race.num", levels2 = -1) +
#         # idegree(indeg.terms)+
#         # odegree(deg.terms)+
#         dist(dist.terms),
#       target.stats =
#         c(
#           edges_target,
#           sex_mixing_align_order,
#           age_mixing_align_order,
#           target_race_num,
#           # c(negbin_inedges$n_nodes[c(indeg.terms+1)]),
#           # c(outedges$n_nodes[c(deg.terms+1)])
#           c(dist_nedge_distribution[dist.terms])
#         ),
#       eval.loglik = FALSE,
#       control = control.ergm(
#         MCMLE.maxit = 500,
#         main.method = c("Stochastic-Approximation"),
#         MCMC.interval = 1e6,
#         MCMC.samplesize = 1e6,
#         MCMLE.termination = "Hotelling",
#         MCMC.effectiveSize = NULL,
#         SAN = control.san(
#           SAN.maxit = 500,
#           SAN.nsteps = 1e8
#         )
#       )
#     )
#   ))

# net_stepwise_dist <- load_or_run(
#   "net.stepwise.dist",
#   quote(simulate(fit.stepwise.dist, nsim = 1))
# )

# net_stepwise_dist

# fit.stepwise.dist.odeg.0 <-
#   load_or_run("fit.stepwise.dist.odeg.0", quote(
#     ergm(
#       net_stepwise_dist ~
#         edges +
#         nodemix("sex", levels2 = -1) +
#         nodemix("young", levels2 = -1) +
#         nodemix("race.num", levels2 = -1) +
#         # idegree(indeg.terms)+
#         odegree(deg.terms.0) +
#         dist(dist.terms),
#       target.stats =
#         c(
#           edges_target,
#           sex_mixing_align_order,
#           age_mixing_align_order,
#           target_race_num,
#           # c(negbin_inedges$n_nodes[c(indeg.terms+1)]),
#           c(outdegree_data$mean_n[c(deg.terms.0 + 1)]),
#           c(dist_nedge_distribution[dist.terms])
#         ),
#       eval.loglik = FALSE,
#       control = control.ergm(
#         MCMLE.maxit = 500,
#         main.method = c("Stochastic-Approximation"),
#         MCMC.interval = 1e6,
#         MCMC.samplesize = 1e6,
#         MCMLE.termination = "Hotelling",
#         MCMC.effectiveSize = NULL,
#         SAN = control.san(
#           SAN.maxit = 500,
#           SAN.nsteps = 1e8
#         )
#       )
#     )
#   ))

# net_fit_stepwise_dist_odeg0 <- load_or_run("net_fit_stepwise_dist_odeg0", quote(
#   simulate(fit.stepwise.dist.odeg.0, nsim = 1)
# ))
# net_fit_stepwise_dist_odeg0

# fit.stepwise.dist.odeg.0.1 <-
#   load_or_run("fit.stepwise.dist.odeg.0.1", quote(
#     ergm(
#       net_fit_stepwise_dist_odeg0 ~
#         edges +
#         nodemix("sex", levels2 = -1) +
#         nodemix("young", levels2 = -1) +
#         nodemix("race.num", levels2 = -1) +
#         # idegree(indeg.terms)+
#         odegree(deg.terms.0_1) +
#         dist(dist.terms),
#       target.stats =
#         c(
#           edges_target,
#           sex_mixing_align_order,
#           age_mixing_align_order,
#           target_race_num,
#           # c(negbin_inedges$n_nodes[c(indeg.terms+1)]),
#           c(outdegree_data$mean_n[c(deg.terms.0_1 + 1)]),
#           c(dist_nedge_distribution[dist.terms])
#         ),
#       eval.loglik = FALSE,
#       control = control.ergm(
#         MCMLE.maxit = 500,
#         main.method = c("Stochastic-Approximation"),
#         MCMC.interval = 1e6,
#         MCMC.samplesize = 1e6,
#         MCMLE.termination = "Hotelling",
#         MCMC.effectiveSize = NULL,
#         SAN = control.san(
#           SAN.maxit = 500,
#           SAN.nsteps = 1e8
#         )
#       )
#     )
#   ))

# net_fit_stepwise_dist_odeg0_1 <- 
# load_or_run("net_fit_stepwise_dist_odeg0_1", quote(
#   simulate(fit.stepwise.dist.odeg.0.1, nsim = 1)
# ))
# net_fit_stepwise_dist_odeg0_1

# fit.stepwise.dist.odeg.0.2 <-
#   load_or_run("fit.stepwise.dist.odeg.0.2", quote(
#     ergm(
#       net_fit_stepwise_dist_odeg0_1 ~
#         edges +
#         nodemix("sex", levels2 = -1) +
#         nodemix("young", levels2 = -1) +
#         nodemix("race.num", levels2 = -1) +
#         # idegree(indeg.terms)+
#         odegree(deg.terms.0_2) +
#         dist(dist.terms),
#       target.stats =
#         c(
#           edges_target,
#           sex_mixing_align_order,
#           age_mixing_align_order,
#           target_race_num,
#           # c(negbin_inedges$n_nodes[c(indeg.terms+1)]),
#           c(outdegree_data$mean_n[c(deg.terms.0_2 + 1)]),
#           c(dist_nedge_distribution[dist.terms])
#         ),
#       eval.loglik = FALSE,
#       control = control.ergm(
#         MCMLE.maxit = 500,
#         main.method = c("Stochastic-Approximation"),
#         MCMC.interval = 1e6,
#         MCMC.samplesize = 1e6,
#         MCMLE.termination = "Hotelling",
#         MCMC.effectiveSize = NULL,
#         SAN = control.san(
#           SAN.maxit = 500,
#           SAN.nsteps = 1e8
#         )
#       )
#     )
#   ))

# net_fit_stepwise_dist_odeg0_2 <- 
# load_or_run("net_fit_stepwise_dist_odeg0_2", quote(
#   simulate(fit.stepwise.dist.odeg.0.2, nsim = 1)
# ))
# net_fit_stepwise_dist_odeg0_2 #this is degenerate

# fit.stepwise.dist.odeg.013 <-
#   load_or_run("fit.stepwise.dist.odeg.0.2", quote(
#     ergm(
#       net_fit_stepwise_dist_odeg0_1 ~
#         edges +
#         nodemix("sex", levels2 = -1) +
#         nodemix("young", levels2 = -1) +
#         nodemix("race.num", levels2 = -1) +
#         # idegree(indeg.terms)+
#         odegree(deg.terms.013) +
#         dist(dist.terms),
#       target.stats =
#         c(
#           edges_target,
#           sex_mixing_align_order,
#           age_mixing_align_order,
#           target_race_num,
#           # c(negbin_inedges$n_nodes[c(indeg.terms+1)]),
#           c(outdegree_data$mean_n[c(deg.terms.013 + 1)]),
#           c(dist_nedge_distribution[dist.terms])
#         ),
#       eval.loglik = FALSE,
#       control = control.ergm(
#         MCMLE.maxit = 500,
#         main.method = c("Stochastic-Approximation"),
#         MCMC.interval = 1e6,
#         MCMC.samplesize = 1e6,
#         MCMLE.termination = "Hotelling",
#         MCMC.effectiveSize = NULL,
#         SAN = control.san(
#           SAN.maxit = 500,
#           SAN.nsteps = 1e8
#         )
#       )
#     )
#   ))

# net_fit_stepwise_dist_odeg013 <- 
# load_or_run("net_fit_stepwise_dist_odeg013", quote(
#   simulate(fit.stepwise.dist.odeg.013, nsim = 1)
# ))
# net_fit_stepwise_dist_odeg013 #this is degenerate


# fit.stepwise.dist.odeg.01.indeg0 <-
#   load_or_run("fit.stepwise.dist.odeg.01.indeg0", quote(
#     ergm(
#       net_fit_stepwise_dist_odeg0_1 ~
#         edges +
#         nodemix("sex", levels2 = -1) +
#         nodemix("young", levels2 = -1) +
#         nodemix("race.num", levels2 = -1) +
#         idegree(indeg.terms.0)+
#         odegree(deg.terms.0_1) +
#         dist(dist.terms),
#       target.stats =
#         c(
#           edges_target,
#           sex_mixing_align_order,
#           age_mixing_align_order,
#           target_race_num,
#           c(indegree_data$mean_n[c(indeg.terms.0 + 1)]),
#           c(outdegree_data$mean_n[c(deg.terms.0_1 + 1)]),
#           c(dist_nedge_distribution[dist.terms])
#         ),
#       eval.loglik = FALSE,
#       control = control.ergm(
#         MCMLE.maxit = 500,
#         main.method = c("Stochastic-Approximation"),
#         MCMC.interval = 1e6,
#         MCMC.samplesize = 1e6,
#         MCMLE.termination = "Hotelling",
#         MCMC.effectiveSize = NULL,
#         SAN = control.san(
#           SAN.maxit = 500,
#           SAN.nsteps = 1e8
#         )
#       )
#     )
#   ))

# net_fit_stepwise_dist_odeg01_indeg0 <- 
# load_or_run("net_fit_stepwise_dist_odeg01_indeg0", quote(
#   simulate(fit.stepwise.dist.odeg.01.indeg0, nsim = 1)
# ))
# net_fit_stepwise_dist_odeg01_indeg0 

# ###

# fit.stepwise.dist.odeg.01.indeg <-
#   load_or_run("fit.stepwise.dist.odeg.01.indeg", quote(
#     ergm(
#       net_fit_stepwise_dist_odeg0_1 ~
#         edges +
#         nodemix("sex", levels2 = -1) +
#         nodemix("young", levels2 = -1) +
#         nodemix("race.num", levels2 = -1) +
#         idegree(indeg.terms)+
#         odegree(deg.terms.0_1) +
#         dist(dist.terms),
#       target.stats =
#         c(
#           edges_target,
#           sex_mixing_align_order,
#           age_mixing_align_order,
#           target_race_num,
#           c(indegree_data$mean_n[c(indeg.terms + 1)]),
#           c(outdegree_data$mean_n[c(deg.terms.0_1 + 1)]),
#           c(dist_nedge_distribution[dist.terms])
#         ),
#       eval.loglik = FALSE,
#       control = control.ergm(
#         MCMLE.maxit = 500,
#         main.method = c("Stochastic-Approximation"),
#         MCMC.interval = 1e6,
#         MCMC.samplesize = 1e6,
#         MCMLE.termination = "Hotelling",
#         MCMC.effectiveSize = NULL,
#         SAN = control.san(
#           SAN.maxit = 500,
#           SAN.nsteps = 1e8
#         )
#       )
#     )
#   ))

# net_fit_stepwise_dist_odeg01_indeg <- 
# load_or_run("net_fit_stepwise_dist_odeg01_indeg", quote(
#   simulate(fit.stepwise.dist.odeg.01.indeg0, nsim = 1)
# ))
# net_fit_stepwise_dist_odeg01_indeg0 

# ## save.image(file=here("fit-ergms", "out", "stepwise-refactored-std-order-2025-jan23-targets.RData"))
# save.image(file = file.path(out_dir, paste0(run_label, ".RData")))
