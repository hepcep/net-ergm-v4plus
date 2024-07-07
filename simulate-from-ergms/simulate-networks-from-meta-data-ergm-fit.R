# Simulate network (n=32K) from ERGM fit 
# parameterized with meta data  


rm(list=ls())

# Initiate environment ------------------------------

library(renv)
renv::activate()

.libPaths()


# Libraries ----------

library(network)
library(ergm)
library(dplyr)
library(ergm.userterms)
library(here)


# Data ----------

load(here("fit-ergms", "out", "updated-with-oct12-2024-synthpop-ergmv4-6-all-plosone-terms.RData"))

#load(here("fit-ergms", "out", "updated-with-july25-2022-synthpop-ergmv4-6-all-plosone-terms-checkpointing-increased-san-and-mcmc.RData"))

# Model summary
summary(fit.metadata.mixing)


# Simulate 100 networks ----------

#nsim.vec <- 1:100
nsim.vec <- 1:5
sim_results <- as.list(nsim.vec)
set.seed(Sys.time())

for (iter in 1:length(nsim.vec)){
  sim_results[[iter]] <- simulate(fit.metadata.mixing,
                                  nsim=1
                                  )
}


#  Investigate netstats on 100 networks ----------

## edgecount
ecount <- unlist(lapply(sim_results, network.edgecount))
summary(ecount)
edges_target

## outdegree
outdeg0 <- unlist(lapply(sim_results, 
                        function (x) summary(x ~ odegree(0))
                        ))
outdeg1 <- unlist(lapply(sim_results, 
                         function (x) summary(x ~ odegree(1))
))
outdeg2 <- unlist(lapply(sim_results, 
                         function (x) summary(x ~ odegree(2))
))
outdeg3 <- unlist(lapply(sim_results, 
                         function (x) summary(x ~ odegree(3))
))
outdeg4 <- unlist(lapply(sim_results, 
                         function (x) summary(x ~ odegree(4))
))


c(mean(outdeg0), mean(outdeg1), mean(outdeg2), mean(outdeg3), mean(outdeg4))
outedges$n_nodes[1:5] #target

## indegree
indeg0 <- unlist(lapply(sim_results, 
                         function (x) summary(x ~ idegree(0))
))
indeg1 <- unlist(lapply(sim_results, 
                         function (x) summary(x ~ idegree(1))
))
indeg2 <- unlist(lapply(sim_results, 
                         function (x) summary(x ~ idegree(2))
))
indeg3 <- unlist(lapply(sim_results, 
                         function (x) summary(x ~ idegree(3))
))
indeg4 <- unlist(lapply(sim_results, 
                         function (x) summary(x ~ idegree(4))
))


c(mean(indeg0), mean(indeg1), mean(indeg2), mean(indeg3), mean(indeg4))
inedges$n_nodes[1:5]

## nodemix(race.num)
race.num <- unlist(lapply(sim_results, 
                         function (x) summary(x ~ nodemix("race.num"))
))

summary(sim_results[[1]] ~ nodemix("race.num"))
round(
  c(target.w.w, target.b.w, target.h.w, target.o.w,
    target.w.b, target.b.b, target.h.b, target.o.b,
    target.w.h, target.b.h, target.h.h, target.o.h,
    target.w.o, target.b.o, target.h.o, target.o.o),
  0
)

## nodemix(sex)
gender <- unlist(lapply(sim_results, 
                          function (x) summary(x ~ nodemix("sex"))
))

round(c(tgt.female.pctfemale, tgt.female.pctmale, tgt.male.pctfemale, tgt.male.pctmale), 0)

## nodemix(young)
young <- unlist(lapply(sim_results, 
                        function (x) summary(x ~ nodemix("young"))
)) 

#summary(sim_results[[10]] ~ nodemix("young"))
round(c(tgt.old.pctold, tgt.old.pctyoung, tgt.young.pctold, tgt.young.pctyoung))

save.image(here("simulate-from-ergms", "out", "simulated-with-all-plos-terms.RData"))
