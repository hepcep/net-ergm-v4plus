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
 negbin_inedges <- data_objects$negbin_inedges   
 target_race_num <- data_objects$target_race_num      
 outedges <- data_objects$outedges
 dist_nedge_distribution <- data_objects$dist_nedge_distribution



# Degree and dist assignments ---------
deg.terms <- 0:3
indeg.terms.0 <- 0
indeg.terms <- 0:1  

dist.terms <- 1:3 #fourth is left out


# Define file paths for saved fitted ERGMs ---------

edges_indeg_model_path <- here("fit-ergms", "out", "fit_edges_indegree.rds")
edges_indeg_outdeg_model_path <- here("fit-ergms", "out", "fit_edges_indegree_outdegree.rds")


# Define filepaths for saved simulated networks ---------

sim_edges_indeg_path <- here("fit-ergms", "out", "sim_edges_indeg.rds")
sim_edges_indeg_outdeg_path <- here("fit-ergms", "out", "sim_edges_indeg_outdeg.rds")



# Edges+Indeg ---------

if (file.exists(edges_indeg_model_path)) {
  fit_edges_indegree <- readRDS(edges_indeg_model_path)
  message("Loaded fit_edges_indegree from file.")
} else {
  fit_edges_indegree <- 
      ergm(
          edges_only_net ~
          edges + 
          idegree(indeg.terms),
          target.stats = 
          c(
          edges_target,
        c(negbin_inedges$n_nodes[c(indeg.terms+1)])
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
  saveRDS(fit_edges_indegree, edges_indeg_model_path)
  message("Saved fit_edges_indegree to file.")
}


if (file.exists(sim_edges_indeg_path)) {
  net_fit_edges_indeg <- readRDS(sim_edges_indeg_path)
  message("Loaded simulated network net_fit_edges_indeg from file.")
} else {
  net_fit_edges_indeg <- simulate(fit_edges_indegree, nsim = 1)
  saveRDS(net_fit_edges_indeg, sim_edges_indeg_path)
  message("Saved simulated network net_fit_edges_indeg to file.")
}


# Edges+Indeg+OutDeg ---------

if (file.exists(edges_indeg_outdeg_model_path)) {
    fit_edges_indegree_outdegree <- readRDS(edges_indeg_outdeg_model_path)
    message("Loaded fit_edges_indegree_outdegree from file.")
} else {
  fit_edges_indegree_outdegree <- 
      ergm(
           net_fit_edges_indeg ~
          edges + 
          idegree(indeg.terms)+
          odegree(deg.terms),
          target.stats = 
          c(
          edges_target,
        c(negbin_inedges$n_nodes[c(indeg.terms+1)]),
          c(outedges$n_nodes[c(deg.terms+1)])
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
  saveRDS(fit_edges_indegree_outdegree, edges_indeg_outdeg_model_path)
  message("Saved fit_edges_indegree_outdegree to file.")
}

if (file.exists(sim_edges_indeg_outdeg_path)) {
  net_fit_edges_indeg_outdeg <- readRDS(sim_edges_indeg_outdeg_path)
  message("Loaded simulated network net_fit_edges_indeg_outdeg from file.")
} else {
  net_fit_edges_indeg_outdeg <- simulate(fit_edges_indegree_outdegree, nsim = 1)
  saveRDS(net_fit_edges_indeg_outdeg, sim_edges_indeg_outdeg_path)
  message("Saved simulated network net_fit_edges_indeg_outdeg to file.")
}



# Save RData ----------

save.image(file=here("fit-ergms", "out", "edges_indeg_first.RData"))  
