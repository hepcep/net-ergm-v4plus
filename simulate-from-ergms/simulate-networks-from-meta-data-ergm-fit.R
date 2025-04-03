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
library(qs)


# Data ----------

## fit output
  load(here("fit-ergms", "out", 
      "stepwise-refactored-checkpointing-data-dated-2025-jan23",
      "stepwise-refactored-checkpointing-data-dated-2025-jan23.RData"
      )
  )

## input params
  data_objects <- readRDS(here("fit-ergms", "out", "processed_data.rds"))
  names(data_objects)

## confirm which run
  run_label

# Model summary
  summary(fit.stepwise.dist.odeg.01.indeg0)


# Simulate 100 networks ----------

  nsim.vec <- 1:10

  sim_results <- as.list(nsim.vec)
  set.seed(Sys.time())

  for (iter in 1:length(nsim.vec)){
    sim_results[[iter]] <- simulate(
      fit.stepwise.dist.odeg.01.indeg0,
      nsim=1
    )
  }


#  Investigate netstats on 100 networks ----------

## edgecount
  ecount <- unlist(lapply(sim_results, network.edgecount))
  summary(ecount)

  mean_edges <- mean(ecount)
  range_edges <- range(ecount)
  target_stats_edges <- edges_target

comparison_df_edges <- data.frame(
  Parameter = c("edges"),
  Mean = mean_edges,
  Range_Min = range_edges[seq(1, length(range_edges), 2)],
  Range_Max = range_edges[seq(2, length(range_edges), 2)],
  Target = target_stats_edges
)

comparison_df_edges

## outdegree
  outdeg0 <- unlist(lapply(sim_results, 
                          function (x) summary(x ~ odegree(0))
                          ))
  outdeg1 <- unlist(lapply(sim_results, 
                          function (x) summary(x ~ odegree(1))
  ))
  # outdeg2 <- unlist(lapply(sim_results, 
  #                         function (x) summary(x ~ odegree(2))
  # ))
  # outdeg3 <- unlist(lapply(sim_results, 
  #                         function (x) summary(x ~ odegree(3))
  # ))
  # outdeg4 <- unlist(lapply(sim_results, 
  #                         function (x) summary(x ~ odegree(4))
  # ))


mean_outdeg <- c(mean(outdeg0), mean(outdeg1))
range_outdeg <- c(range(outdeg0), range(outdeg1))

target_stats_outdeg <- outdegree_data$mean_n[1:2] #target

comparison_df_outdeg <- data.frame(
  Parameter = c("outdeg0", "outdeg1"),
  Mean = mean_outdeg,
  Range_Min = range_outdeg[seq(1, length(range_outdeg), 2)],
  Range_Max = range_outdeg[seq(2, length(range_outdeg), 2)],
  Target = target_stats_outdeg
)

comparison_df_outdeg

## indegree
indeg0 <- unlist(lapply(sim_results, 
                         function (x) summary(x ~ idegree(0))
))
indeg1 <- unlist(lapply(sim_results, 
                         function (x) summary(x ~ idegree(1))
))

# indeg2 <- unlist(lapply(sim_results, 
#                          function (x) summary(x ~ idegree(2))
# ))
# indeg3 <- unlist(lapply(sim_results, 
#                          function (x) summary(x ~ idegree(3))
# ))
# indeg4 <- unlist(lapply(sim_results, 
#                          function (x) summary(x ~ idegree(4))
# ))


c(mean(indeg0), mean(indeg1))
indegree_data$mean_n[1:2]
sum(indegree_data$mean_n[1:2])

mean_indeg <- c(mean(indeg0), mean(indeg1))
range_indeg <- c(range(indeg0), range(indeg1))

target_stats_indeg <- indegree_data$mean_n[1:2] #target

comparison_df_indeg <- data.frame(
  Parameter = c("indeg0", "indeg1"),
  Mean = mean_indeg,
  Range_Min = range_indeg[seq(1, length(range_indeg), 2)],
  Range_Max = range_indeg[seq(2, length(range_indeg), 2)],
  Target = target_stats_indeg
)

comparison_df_indeg
sum(comparison_df_indeg$Mean)

## nodemix(race.num)
race.num <- unlist(lapply(sim_results, 
                         function (x) summary(x ~ nodemix("race.num"))
))

summary(sim_results[[1]] ~ nodemix("race.num"))



round(target_race_num, 2) # no w.w.
sum(target_race_num)

## nodemix(sex)
gender <- unlist(lapply(sim_results, 
                          function (x) summary(x ~ nodemix("sex"))
))
gender

round(c(tgt.female.pctmale, tgt.male.pctfemale, tgt.male.pctmale), 0)

## nodemix(young)
young <- unlist(lapply(sim_results, 
                        function (x) summary(x ~ nodemix("young"))
)) 
young

#summary(sim_results[[10]] ~ nodemix("young"))
round(c(tgt.old.pctyoung, tgt.young.pctold, tgt.young.pctyoung))


## dist

dist_sim <- 
  unlist(lapply(sim_results, 
                            function (x) summary(x ~ dist(dist.terms))
  ))
dist_sim

round(dist.nedge.distribution[dist.terms])

# save.image(here(
#   "simulate-from-ergms", "out", 
#   paste0(run_label, "_10_for_development", ".RData")))


qsave(sim_results, 
here("simulate-from-ergms", "out", paste0(run_label, "_sim_results_10.qs")))

# save.image(here(
#   "simulate-from-ergms", "out", 
#   paste0(run_label, "_10_for_development", ".RData")))

