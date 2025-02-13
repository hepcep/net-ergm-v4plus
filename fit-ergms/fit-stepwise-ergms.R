# Analyze synthetic dataset with 32K nodes 


# Fit ERGM with 5 dyadic independent terms

rm(list=ls())

# Activate R environment ----------

library(renv)
renv::activate()


# Libraries ----------

library(network)
library(ergm)
library(dplyr)
library(ergm.userterms)
library(here)


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

inedges_target <- indegree_data$in_degree*indegree_data$mean_n
sum(inedges_target)

outedges_target <- outdegree_data$out_degree*outdegree_data$mean_n
sum(outedges_target)

# Degree and dist assignments ---------
deg.terms <- 0:3
indeg.terms.0 <- 0
indeg.terms <- 0:1  

dist.terms <- 1:3 #fourth is left out

class(target_race_num)
target_race_num <- unname(target_race_num)

# Fit Non-empty net including race term ---------

fit_nonempty_network_w_race_num <- 
  ergm(
    edges_only_net ~
      edges + 
      nodemix("sex", levels2=-1)+
      nodemix("young", levels2=-1)+
      nodemix("race.num", levels2=-1),
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
      MCMC.effectiveSize=NULL,
      SAN = control.san(
        SAN.maxit = 500, 
        SAN.nsteps = 1e8
      )
    )
    )

non_empty_net_w_race_term <- simulate(fit_nonempty_network_w_race_num, nsim=1)
non_empty_net_w_race_term

fit.stepwise.dist <-
  ergm(
    non_empty_net_w_race_term ~
      edges + 
      nodemix("sex", levels2=-1)+
      nodemix("young", levels2=-1)+
      nodemix("race.num", levels2=-1)+
      #idegree(indeg.terms)+
      #odegree(deg.terms)+
      dist(dist.terms),
    target.stats = 
    c(
      edges_target,
      c(tgt.female.pctmale, tgt.male.pctfemale, tgt.male.pctmale),           
      c(tgt.old.pctyoung, tgt.young.pctold, tgt.young.pctyoung),
      target_race_num,
      #c(negbin_inedges$n_nodes[c(indeg.terms+1)]),
      #c(outedges$n_nodes[c(deg.terms+1)])
      c(dist.nedge.distribution[dist.terms])
    ),
    eval.loglik = FALSE,
    control = control.ergm(
      MCMLE.maxit = 500,
      main.method = c("Stochastic-Approximation"),
      MCMC.interval = 1e6,
      MCMC.samplesize = 1e6,
      MCMLE.termination = "Hotelling",
      MCMC.effectiveSize=NULL,
      SAN = control.san(
        SAN.maxit = 500, 
        SAN.nsteps = 1e8
      )
    )
                           
    )
  
net_stepwise_dist <- simulate(fit.stepwise.dist, nsim=1)
net_stepwise_dist

fit.stepwise.dist.odeg <-
  ergm(
    net_stepwise_dist ~
      edges + 
      nodemix("sex", levels2=-1)+
      nodemix("young", levels2=-1)+
      nodemix("race.num", levels2=-1)+
      #idegree(indeg.terms)+
      odegree(deg.terms)+
      dist(dist.terms),
    target.stats = 
    c(
      edges_target,
      c(tgt.female.pctmale, tgt.male.pctfemale, tgt.male.pctmale),           
      c(tgt.old.pctyoung, tgt.young.pctold, tgt.young.pctyoung),
      target_race_num,
      #c(negbin_inedges$n_nodes[c(indeg.terms+1)]),
      c(outdegree_data$mean_n[c(deg.terms+1)]),
      c(dist.nedge.distribution[dist.terms])
    ),
    eval.loglik = FALSE,
    control = control.ergm(
      MCMLE.maxit = 500,
      main.method = c("Stochastic-Approximation"),
      MCMC.interval = 1e6,
      MCMC.samplesize = 1e6,
      MCMLE.termination = "Hotelling",
      MCMC.effectiveSize=NULL,
      SAN = control.san(
        SAN.maxit = 500, 
        SAN.nsteps = 1e8
      )
    )                         
    )

  net_fit_stepwise_dist_odeg <- simulate(fit.stepwise.dist.odeg, nsim=1)
  net_fit_stepwise_dist_odeg

fit.stepwise.dist.odeg.ideg0 <-
  ergm(
    net_fit_stepwise_dist_odeg ~
      edges + 
      nodemix("sex", levels2=-1)+
      nodemix("young", levels2=-1)+
      nodemix("race.num", levels2=-1)+
      idegree(indeg.terms.0)+
      odegree(deg.terms)+
      dist(dist.terms),
    target.stats = 
    c(
      edges_target,
      c(tgt.female.pctmale, tgt.male.pctfemale, tgt.male.pctmale),           
      c(tgt.old.pctyoung, tgt.young.pctold, tgt.young.pctyoung),
      target_race_num,
      c(indegree_data$mean_n[c(indeg.terms.0+1)]),
      c(outdegree_data$mean_n[c(deg.terms+1)])),
      c(dist.nedge.distribution[dist.terms])
    ),
    eval.loglik = FALSE,
    control = control.ergm(
      MCMLE.maxit = 500,
      main.method = c("Stochastic-Approximation"),
      MCMC.interval = 1e6,
      MCMC.samplesize = 1e6,
      MCMLE.termination = "Hotelling",
      MCMC.effectiveSize=NULL,
      SAN = control.san(
        SAN.maxit = 500, 
        SAN.nsteps = 1e8
      )
    )                         
    )

net_fit_stepwise_dist_odeg_ideg0 <- simulate(fit.stepwise.dist.odeg.ideg0, nsim=1)
net_fit_stepwise_dist_odeg_ideg0


fit.stepwise.dist.odeg.ideg <-
  ergm(
    net_fit_stepwise_dist_odeg_ideg0 ~
      edges + 
      nodemix("sex", levels2=-1)+
      nodemix("young", levels2=-1)+
      nodemix("race.num", levels2=-1)+
      idegree(indeg.terms)+
      odegree(deg.terms)+
      dist(dist.terms),
    target.stats = 
    c(
      edges_target,
      c(tgt.female.pctmale, tgt.male.pctfemale, tgt.male.pctmale),           
      c(tgt.old.pctyoung, tgt.young.pctold, tgt.young.pctyoung),
      target_race_num,
      c(indegree_data$mean_n[c(indeg.terms.0+1)]),,
      c(outdegree_data$mean_n[c(deg.terms+1)])),
      c(dist.nedge.distribution[dist.terms])
    ),
    eval.loglik = FALSE,
    control = control.ergm(
      MCMLE.maxit = 500,
      main.method = c("Stochastic-Approximation"),
      MCMC.interval = 1e6,
      MCMC.samplesize = 1e6,
      MCMLE.termination = "Hotelling",
      MCMC.effectiveSize=NULL,
      SAN = control.san(
        SAN.maxit = 500, 
        SAN.nsteps = 1e8
      )
    )                         
    )

net_fit_stepwise_dist_odeg_ideg <-simulate(fit.stepwise.dist.odeg.ideg, nsim=1)
net_fit_stepwise_dist_odeg_ideg

save.image(file=here("fit-ergms", "out", "stepwise-refactored-std-order-2025-jan23-targets.RData"))  
