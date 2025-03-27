# Analyze synthetic dataset with 32K nodes


# Fit ERGM with 5 dyadic independent terms

rm(list = ls())

# Label and outputs for this run
run_label <- "stepwise-refactored-checkpointing-data-dated-2025-jan23" # set manually to ensure intentional updates

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

data <- data_objects$data
n0 <- data_objects$n0
edges_only_net <- data_objects$edges_only_net
edges_target <- data_objects$edges_target
tgt.female.pctmale <- data_objects$tgt.female.pctmale
tgt.male.pctfemale <- data_objects$tgt.male.pctfemale
tgt.male.pctmale <- data_objects$tgt.male.pctmale
tgt.old.pctyoung <- data_objects$tgt.old.pctyoung
tgt.young.pctold <- data_objects$tgt.young.pctold
tgt.young.pctyoung <- data_objects$tgt.young.pctyoung
indegree_data <- data_objects$indegree_data
target_race_num <- data_objects$target_race_num
outdegree_data <- data_objects$outdegree_data
dist.nedge.distribution <- data_objects$dist_nedge_distribution

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

fit_nonempty_network_w_race_num <-
  load_or_run("fit_nonempty_network_w_race_num", quote(
    ergm(
      edges_only_net ~
        edges +
        nodemix("sex", levels2 = -1) +
        nodemix("young", levels2 = -1) +
        nodemix("race.num", levels2 = -1),
      target.stats =
        c(
          edges_target,
          c(tgt.female.pctmale, tgt.male.pctfemale, tgt.male.pctmale),
          c(tgt.old.pctyoung, tgt.young.pctold, tgt.young.pctyoung),
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
  ))

non_empty_net_w_race_term <- load_or_run("net_nonempty_w_race_term", quote(
  simulate(fit_nonempty_network_w_race_num, nsim = 1)
))
non_empty_net_w_race_term

fit.stepwise.dist <-
  load_or_run("fit.stepwise.dist", quote(
    ergm(
      non_empty_net_w_race_term ~
        edges +
        nodemix("sex", levels2 = -1) +
        nodemix("young", levels2 = -1) +
        nodemix("race.num", levels2 = -1) +
        # idegree(indeg.terms)+
        # odegree(deg.terms)+
        dist(dist.terms),
      target.stats =
        c(
          edges_target,
          c(tgt.female.pctmale, tgt.male.pctfemale, tgt.male.pctmale),
          c(tgt.old.pctyoung, tgt.young.pctold, tgt.young.pctyoung),
          target_race_num,
          # c(negbin_inedges$n_nodes[c(indeg.terms+1)]),
          # c(outedges$n_nodes[c(deg.terms+1)])
          c(dist.nedge.distribution[dist.terms])
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

net_stepwise_dist <- load_or_run(
  "net.stepwise.dist",
  quote(simulate(fit.stepwise.dist, nsim = 1))
)

net_stepwise_dist

fit.stepwise.dist.odeg.0 <-
  load_or_run("fit.stepwise.dist.odeg.0", quote(
    ergm(
      net_stepwise_dist ~
        edges +
        nodemix("sex", levels2 = -1) +
        nodemix("young", levels2 = -1) +
        nodemix("race.num", levels2 = -1) +
        # idegree(indeg.terms)+
        odegree(deg.terms.0) +
        dist(dist.terms),
      target.stats =
        c(
          edges_target,
          c(tgt.female.pctmale, tgt.male.pctfemale, tgt.male.pctmale),
          c(tgt.old.pctyoung, tgt.young.pctold, tgt.young.pctyoung),
          target_race_num,
          # c(negbin_inedges$n_nodes[c(indeg.terms+1)]),
          c(outdegree_data$mean_n[c(deg.terms.0 + 1)]),
          c(dist.nedge.distribution[dist.terms])
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

net_fit_stepwise_dist_odeg0 <- load_or_run("net_fit_stepwise_dist_odeg0", quote(
  simulate(fit.stepwise.dist.odeg.0, nsim = 1)
))
net_fit_stepwise_dist_odeg0

fit.stepwise.dist.odeg.0.1 <-
  load_or_run("fit.stepwise.dist.odeg.0.1", quote(
    ergm(
      net_fit_stepwise_dist_odeg0 ~
        edges +
        nodemix("sex", levels2 = -1) +
        nodemix("young", levels2 = -1) +
        nodemix("race.num", levels2 = -1) +
        # idegree(indeg.terms)+
        odegree(deg.terms.0_1) +
        dist(dist.terms),
      target.stats =
        c(
          edges_target,
          c(tgt.female.pctmale, tgt.male.pctfemale, tgt.male.pctmale),
          c(tgt.old.pctyoung, tgt.young.pctold, tgt.young.pctyoung),
          target_race_num,
          # c(negbin_inedges$n_nodes[c(indeg.terms+1)]),
          c(outdegree_data$mean_n[c(deg.terms.0_1 + 1)]),
          c(dist.nedge.distribution[dist.terms])
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

net_fit_stepwise_dist_odeg0_1 <- 
load_or_run("net_fit_stepwise_dist_odeg0_1", quote(
  simulate(fit.stepwise.dist.odeg.0.1, nsim = 1)
))
net_fit_stepwise_dist_odeg0_1

fit.stepwise.dist.odeg.0.2 <-
  load_or_run("fit.stepwise.dist.odeg.0.2", quote(
    ergm(
      net_fit_stepwise_dist_odeg0_1 ~
        edges +
        nodemix("sex", levels2 = -1) +
        nodemix("young", levels2 = -1) +
        nodemix("race.num", levels2 = -1) +
        # idegree(indeg.terms)+
        odegree(deg.terms.0_2) +
        dist(dist.terms),
      target.stats =
        c(
          edges_target,
          c(tgt.female.pctmale, tgt.male.pctfemale, tgt.male.pctmale),
          c(tgt.old.pctyoung, tgt.young.pctold, tgt.young.pctyoung),
          target_race_num,
          # c(negbin_inedges$n_nodes[c(indeg.terms+1)]),
          c(outdegree_data$mean_n[c(deg.terms.0_2 + 1)]),
          c(dist.nedge.distribution[dist.terms])
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

net_fit_stepwise_dist_odeg0_2 <- 
load_or_run("net_fit_stepwise_dist_odeg0_2", quote(
  simulate(fit.stepwise.dist.odeg.0.2, nsim = 1)
))
net_fit_stepwise_dist_odeg0_2 #this is degenerate

fit.stepwise.dist.odeg.013 <-
  load_or_run("fit.stepwise.dist.odeg.0.2", quote(
    ergm(
      net_fit_stepwise_dist_odeg0_1 ~
        edges +
        nodemix("sex", levels2 = -1) +
        nodemix("young", levels2 = -1) +
        nodemix("race.num", levels2 = -1) +
        # idegree(indeg.terms)+
        odegree(deg.terms.013) +
        dist(dist.terms),
      target.stats =
        c(
          edges_target,
          c(tgt.female.pctmale, tgt.male.pctfemale, tgt.male.pctmale),
          c(tgt.old.pctyoung, tgt.young.pctold, tgt.young.pctyoung),
          target_race_num,
          # c(negbin_inedges$n_nodes[c(indeg.terms+1)]),
          c(outdegree_data$mean_n[c(deg.terms.013 + 1)]),
          c(dist.nedge.distribution[dist.terms])
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

net_fit_stepwise_dist_odeg013 <- 
load_or_run("net_fit_stepwise_dist_odeg013", quote(
  simulate(fit.stepwise.dist.odeg.013, nsim = 1)
))
net_fit_stepwise_dist_odeg013 #this is degenerate

###
fit.stepwise.dist.odeg.01.indeg0 <-
  load_or_run("fit.stepwise.dist.odeg.01.indeg0", quote(
    ergm(
      net_fit_stepwise_dist_odeg0_1 ~
        edges +
        nodemix("sex", levels2 = -1) +
        nodemix("young", levels2 = -1) +
        nodemix("race.num", levels2 = -1) +
        idegree(indeg.terms.0)+
        odegree(deg.terms.0_1) +
        dist(dist.terms),
      target.stats =
        c(
          edges_target,
          c(tgt.female.pctmale, tgt.male.pctfemale, tgt.male.pctmale),
          c(tgt.old.pctyoung, tgt.young.pctold, tgt.young.pctyoung),
          target_race_num,
          c(indegree_data$mean_n[c(indeg.terms.0 + 1)]),
          c(outdegree_data$mean_n[c(deg.terms.0_1 + 1)]),
          c(dist.nedge.distribution[dist.terms])
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

net_fit_stepwise_dist_odeg01_indeg0 <- 
load_or_run("net_fit_stepwise_dist_odeg01_indeg01", quote(
  simulate(fit.stepwise.dist.odeg.01.indeg0, nsim = 1)
))
net_fit_stepwise_dist_odeg01_indeg0 



## save.image(file=here("fit-ergms", "out", "stepwise-refactored-std-order-2025-jan23-targets.RData"))
save.image(file = file.path(out_dir, paste0(run_label, ".RData")))
