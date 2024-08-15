# Summarize simulated statistics across multiple networks


rm(list = ls())

# Initiate environment ------------------------------

library(renv)
renv::activate()

.libPaths()


# Libraries ----------

# library(network)
# library(ergm)
# library(dplyr)
# library(ergm.userterms)
library(here)
library(ggplot2)


# Data ----------


load(here("simulate-from-ergms", "in", "sim-updated-with-oct12-2024-synthpop-ergmv4-6-all-plos1-mcmc-int1e6-samp1e6-hotelling.RData"))

# Compute summaries and IQRs ----------

## edges

### simulated
edgecount.sim.data <- (unlist(lapply(
  sim_results,
  function(x) network.edgecount(x)
))) # edge count summary

mean(edgecount.sim.data)
quantile(edgecount.sim.data, probs = c(2.5 / 100, 97.5 / 100))

### target
edges_target


## indegree
### simulated
summary(indeg0)
quantile(indeg0, probs = c(2.5 / 100, 97.5 / 100))
summary(indeg1)
quantile(indeg1, probs = c(2.5 / 100, 97.5 / 100))
summary(indeg2)
quantile(indeg2, probs = c(2.5 / 100, 97.5 / 100))
summary(indeg3)
quantile(indeg3, probs = c(2.5 / 100, 97.5 / 100))
# summary(indeg4); quantile(indeg4, probs = c(2.5/100, 97.5/100))

indeg.gr.0.3 <- n - (indeg0 + indeg1 + indeg2 + indeg3)
summary(indeg.gr.0.3)
quantile(indeg.gr.0.3, probs = c(2.5 / 100, 97.5 / 100))


### target
target_stats_indeg



## indegree

### simulated
summary(indeg0)
quantile(indeg0, probs = c(2.5 / 100, 97.5 / 100))
summary(indeg1)
quantile(indeg1, probs = c(2.5 / 100, 97.5 / 100))
summary(indeg2)
quantile(indeg2, probs = c(2.5 / 100, 97.5 / 100))
summary(indeg3)
quantile(indeg3, probs = c(2.5 / 100, 97.5 / 100))
summary(indeg4)
quantile(indeg4, probs = c(2.5 / 100, 97.5 / 100))

indeg.gr.0.1 <- n - (indeg0 + indeg1)
summary(indeg.gr.0.1)
quantile(indeg.gr.0.1, probs = c(2.5 / 100, 97.5 / 100))

### target
target_stats_indeg

## race

sim.race.num <- lapply(nsim.vec, function(x) summary(sim_results[[x]] ~ nodemix("race.num")))
summary(unlist(lapply(sim.race.num, function(x) x["mix.race.num.1.1"])))
summary(unlist(lapply(sim.race.num, function(x) x["mix.race.num.2.1"])))
summary(unlist(lapply(sim.race.num, function(x) x["mix.race.num.3.1"])))
summary(unlist(lapply(sim.race.num, function(x) x["mix.race.num.4.1"])))
summary(unlist(lapply(sim.race.num, function(x) x["mix.race.num.1.2"])))
summary(unlist(lapply(sim.race.num, function(x) x["mix.race.num.2.2"])))
summary(unlist(lapply(sim.race.num, function(x) x["mix.race.num.3.2"])))
summary(unlist(lapply(sim.race.num, function(x) x["mix.race.num.4.2"])))
summary(unlist(lapply(sim.race.num, function(x) x["mix.race.num.1.3"])))
summary(unlist(lapply(sim.race.num, function(x) x["mix.race.num.2.3"])))
summary(unlist(lapply(sim.race.num, function(x) x["mix.race.num.3.3"])))
summary(unlist(lapply(sim.race.num, function(x) x["mix.race.num.4.3"])))
summary(unlist(lapply(sim.race.num, function(x) x["mix.race.num.1.4"])))
summary(unlist(lapply(sim.race.num, function(x) x["mix.race.num.2.4"])))
summary(unlist(lapply(sim.race.num, function(x) x["mix.race.num.3.4"])))
summary(unlist(lapply(sim.race.num, function(x) x["mix.race.num.4.4"])))

quantile(unlist(lapply(sim.race.num, function(x) x["mix.race.num.1.1"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.race.num, function(x) x["mix.race.num.2.1"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.race.num, function(x) x["mix.race.num.3.1"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.race.num, function(x) x["mix.race.num.4.1"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.race.num, function(x) x["mix.race.num.1.2"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.race.num, function(x) x["mix.race.num.2.2"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.race.num, function(x) x["mix.race.num.3.2"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.race.num, function(x) x["mix.race.num.4.2"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.race.num, function(x) x["mix.race.num.1.3"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.race.num, function(x) x["mix.race.num.2.3"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.race.num, function(x) x["mix.race.num.3.3"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.race.num, function(x) x["mix.race.num.4.3"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.race.num, function(x) x["mix.race.num.1.4"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.race.num, function(x) x["mix.race.num.2.4"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.race.num, function(x) x["mix.race.num.3.4"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.race.num, function(x) x["mix.race.num.4.4"])), probs = c(2.5 / 100, 97.5 / 100))



sim.gender <- lapply(nsim.vec, function(x) summary(sim_results[[x]] ~ nodemix("gender")))
summary(unlist(lapply(sim.gender, function(x) x["mix.gender.female.female"])))
summary(unlist(lapply(sim.gender, function(x) x["mix.gender.male.female"])))
summary(unlist(lapply(sim.gender, function(x) x["mix.gender.female.male"])))
summary(unlist(lapply(sim.gender, function(x) x["mix.gender.male.male"])))

quantile(unlist(lapply(sim.gender, function(x) x["mix.gender.female.female"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.gender, function(x) x["mix.gender.male.female"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.gender, function(x) x["mix.gender.female.male"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.gender, function(x) x["mix.gender.male.male"])), probs = c(2.5 / 100, 97.5 / 100))


sim.young <- lapply(nsim.vec, function(x) summary(sim_results[[x]] ~ nodemix("young")))
summary(unlist(lapply(sim.young, function(x) x["mix.young.0.0"])))
summary(unlist(lapply(sim.young, function(x) x["mix.young.1.0"])))
summary(unlist(lapply(sim.young, function(x) x["mix.young.0.1"])))
summary(unlist(lapply(sim.young, function(x) x["mix.young.1.1"])))

quantile(unlist(lapply(sim.young, function(x) x["mix.young.0.0"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.young, function(x) x["mix.young.1.0"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.young, function(x) x["mix.young.0.1"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.young, function(x) x["mix.young.1.1"])), probs = c(2.5 / 100, 97.5 / 100))


sim.dist <- lapply(nsim.vec, function(x) summary(sim_results[[x]] ~ dist(1:4)))
summary(unlist(lapply(sim.dist, function(x) x["dist1"])))
summary(unlist(lapply(sim.dist, function(x) x["dist2"])))
summary(unlist(lapply(sim.dist, function(x) x["dist3"])))
summary(unlist(lapply(sim.dist, function(x) x["dist4"])))

quantile(unlist(lapply(sim.dist, function(x) x["dist1"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.dist, function(x) x["dist2"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.dist, function(x) x["dist3"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.dist, function(x) x["dist4"])), probs = c(2.5 / 100, 97.5 / 100))

# Violin Plots ----------

## edges
edgecount_df <- data.frame(
  count = edgecount.sim.data,
  category = "edges"
)

ggplot(edgecount_df, aes(x = category, y = count)) +
  geom_violin(trim = FALSE, fill = "#66C2A5") +
  geom_hline(
    yintercept = edges_target, linetype = "solid",
    color = "black", size = 1.5
  ) +
  scale_y_continuous(
    limits = c(min(edgecount.sim.data) - 1000, max(edgecount.sim.data) + 1000),
    breaks = seq(24000, 27000, by = 1000),
    labels = scales::comma # Format labels with commas
  ) +
  theme_minimal() +
  labs(y = "Edge Count", x = "") +
  theme(
    axis.text.x = element_blank(), # Hide x-axis text
    axis.title.x = element_blank(), # Hide x-axis title
    axis.title.y = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 16, face = "bold") # Make "edges" prominent
  ) +
  ggtitle("EDGES") # Add "edges" as the title

ggsave(here("simulate-from-ergms", "in", "edges_violin_plot.png"), width = 8, height = 6)

## indegree
indeg_df <- data.frame(
  indegree = c(indeg0, indeg1, indeg2, indeg3),
  category = rep(c("indeg0", "indeg1", "indeg2", "indeg3"),
    times = c(length(indeg0), length(indeg1), length(indeg2), length(indeg3))
  )
)

target_values <- target_stats_indeg

ggplot(indeg_df, aes(x = category, y = indegree)) +
  geom_violin(trim = FALSE, fill = "#66C2A5") +
  geom_hline(
    data = data.frame(
      category = c("indeg0", "indeg1", "indeg2", "indeg3"),
      y = target_values
    ), aes(yintercept = y),
    linetype = "solid", color = "black", size = 1.5
  ) +
  facet_wrap(~category, scales = "free_y") +
  theme_minimal() +
  labs(y = "indegree", x = "") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 14, face = "bold")
  )
  ggsave(here("simulate-from-ergms", "in", "indeg_violin_plot.png"), width = 8, height = 6)


## indegree

indeg_df <- data.frame(
  indegree = c(indeg0, indeg1, indeg2, indeg3),
  category = rep(c("indeg0", "indeg1"),
    times = c(length(indeg0), length(indeg1))
  )
)

target_values <- target_stats_indeg[1:2]

ggplot(indeg_df, aes(x = category, y = indegree)) +
  geom_violin(trim = FALSE, fill = "#66C2A5") +
  geom_hline(
    data = data.frame(
      category = c("indeg0", "indeg1", "indeg2", "indeg3"),
      y = target_values
    ), aes(yintercept = y),
    linetype = "solid", color = "black", size = 1.5
  ) +
  facet_wrap(~category, scales = "free_y") +
  theme_minimal() +
  labs(y = "indegree", x = "") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 14, face = "bold")
  )
  ggsave(here("simulate-from-ergms", "out", "indeg_violin_plot.png"), width = 8, height = 6)
