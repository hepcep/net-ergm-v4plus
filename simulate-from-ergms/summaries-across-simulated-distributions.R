# Summarize simulated statistics across multiple networks


rm(list=ls())


# Libraries ----------

library(network)
library(ergm)
library(ergm.userterms)

# Data ----------


load("out/simulate-racemix-plus-dist-plus-negbin-odeg0-3-indeg0-1-orignialdata.RData")

# Compute summaries and IQRs ----------

edgecount.sim.data <- (unlist(lapply(sim_results, function (x) network.edgecount(x)))) #edge count summary
mean(edgecount.sim.data)
quantile(edgecount.sim.data, probs = c(2.5/100, 97.5/100))

summary(outdeg0); quantile(outdeg0, probs = c(2.5/100, 97.5/100))
summary(outdeg1); quantile(outdeg1, probs = c(2.5/100, 97.5/100)) 
summary(outdeg2); quantile(outdeg2, probs = c(2.5/100, 97.5/100))
summary(outdeg3); quantile(outdeg3, probs = c(2.5/100, 97.5/100))
summary(outdeg4); quantile(outdeg4, probs = c(2.5/100, 97.5/100))

summary(indeg0); quantile(indeg0, probs = c(2.5/100, 97.5/100))
summary(indeg1); quantile(indeg1, probs = c(2.5/100, 97.5/100)) 
summary(indeg2); quantile(indeg2, probs = c(2.5/100, 97.5/100))
summary(indeg3); quantile(indeg3, probs = c(2.5/100, 97.5/100))
summary(indeg4); quantile(indeg4, probs = c(2.5/100, 97.5/100))

indeg.gr.0.1 <- n - (indeg0 + indeg1)
summary(indeg.gr.0.1)
quantile(indeg.gr.0.1, probs = c(2.5/100, 97.5/100))

outdeg.gr.0.3 <- n - (outdeg0 + outdeg1 + outdeg2 + outdeg3)
summary(outdeg.gr.0.3)
quantile(outdeg.gr.0.3, probs = c(2.5/100, 97.5/100))

sim.race.num <- lapply(nsim.vec, function (x) summary(sim_results[[x]] ~ nodemix("race.num")))
summary(unlist(lapply(sim.race.num, function (x) x["mix.race.num.1.1"])))
summary(unlist(lapply(sim.race.num, function (x) x["mix.race.num.2.1"])))
summary(unlist(lapply(sim.race.num, function (x) x["mix.race.num.3.1"])))
summary(unlist(lapply(sim.race.num, function (x) x["mix.race.num.4.1"])))
summary(unlist(lapply(sim.race.num, function (x) x["mix.race.num.1.2"])))
summary(unlist(lapply(sim.race.num, function (x) x["mix.race.num.2.2"])))
summary(unlist(lapply(sim.race.num, function (x) x["mix.race.num.3.2"])))
summary(unlist(lapply(sim.race.num, function (x) x["mix.race.num.4.2"])))
summary(unlist(lapply(sim.race.num, function (x) x["mix.race.num.1.3"])))
summary(unlist(lapply(sim.race.num, function (x) x["mix.race.num.2.3"])))
summary(unlist(lapply(sim.race.num, function (x) x["mix.race.num.3.3"])))
summary(unlist(lapply(sim.race.num, function (x) x["mix.race.num.4.3"])))
summary(unlist(lapply(sim.race.num, function (x) x["mix.race.num.1.4"])))
summary(unlist(lapply(sim.race.num, function (x) x["mix.race.num.2.4"])))
summary(unlist(lapply(sim.race.num, function (x) x["mix.race.num.3.4"])))
summary(unlist(lapply(sim.race.num, function (x) x["mix.race.num.4.4"])))

quantile(unlist(lapply(sim.race.num, function (x) x["mix.race.num.1.1"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.race.num, function (x) x["mix.race.num.2.1"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.race.num, function (x) x["mix.race.num.3.1"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.race.num, function (x) x["mix.race.num.4.1"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.race.num, function (x) x["mix.race.num.1.2"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.race.num, function (x) x["mix.race.num.2.2"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.race.num, function (x) x["mix.race.num.3.2"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.race.num, function (x) x["mix.race.num.4.2"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.race.num, function (x) x["mix.race.num.1.3"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.race.num, function (x) x["mix.race.num.2.3"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.race.num, function (x) x["mix.race.num.3.3"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.race.num, function (x) x["mix.race.num.4.3"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.race.num, function (x) x["mix.race.num.1.4"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.race.num, function (x) x["mix.race.num.2.4"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.race.num, function (x) x["mix.race.num.3.4"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.race.num, function (x) x["mix.race.num.4.4"])), probs = c(2.5/100, 97.5/100))



sim.gender <- lapply(nsim.vec, function (x) summary(sim_results[[x]] ~ nodemix("gender")))
summary(unlist(lapply(sim.gender, function (x) x["mix.gender.female.female"])))
summary(unlist(lapply(sim.gender, function (x) x["mix.gender.male.female"])))
summary(unlist(lapply(sim.gender, function (x) x["mix.gender.female.male"])))
summary(unlist(lapply(sim.gender, function (x) x["mix.gender.male.male"])))

quantile(unlist(lapply(sim.gender, function (x) x["mix.gender.female.female"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.gender, function (x) x["mix.gender.male.female"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.gender, function (x) x["mix.gender.female.male"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.gender, function (x) x["mix.gender.male.male"])), probs = c(2.5/100, 97.5/100))


sim.young <- lapply(nsim.vec, function (x) summary(sim_results[[x]] ~ nodemix("young")))
summary(unlist(lapply(sim.young, function (x) x["mix.young.0.0"])))
summary(unlist(lapply(sim.young, function (x) x["mix.young.1.0"])))
summary(unlist(lapply(sim.young, function (x) x["mix.young.0.1"])))
summary(unlist(lapply(sim.young, function (x) x["mix.young.1.1"])))

quantile(unlist(lapply(sim.young, function (x) x["mix.young.0.0"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.young, function (x) x["mix.young.1.0"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.young, function (x) x["mix.young.0.1"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.young, function (x) x["mix.young.1.1"])), probs = c(2.5/100, 97.5/100))


sim.dist <- lapply(nsim.vec, function (x) summary(sim_results[[x]] ~ dist(1:4)))
summary(unlist(lapply(sim.dist, function (x) x["dist1"])))
summary(unlist(lapply(sim.dist, function (x) x["dist2"])))
summary(unlist(lapply(sim.dist, function (x) x["dist3"])))
summary(unlist(lapply(sim.dist, function (x) x["dist4"])))

quantile(unlist(lapply(sim.dist, function (x) x["dist1"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.dist, function (x) x["dist2"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.dist, function (x) x["dist3"])), probs = c(2.5/100, 97.5/100))
quantile(unlist(lapply(sim.dist, function (x) x["dist4"])), probs = c(2.5/100, 97.5/100))

