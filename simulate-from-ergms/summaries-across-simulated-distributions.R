# Summarize simulated statistics across multiple networks


rm(list = ls())

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
library(ggplot2)


# Data ----------


load(here("simulate-from-ergms", "out", "sim-updated-with-oct12-2024-synthpop-ergmv4-6-all-plos1-mcmc-int1e6-samp1e6-hotelling.RData"))

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


## outdegree
### simulated
summary(outdeg0)
quantile(outdeg0, probs = c(2.5 / 100, 97.5 / 100))
summary(outdeg1)
quantile(outdeg1, probs = c(2.5 / 100, 97.5 / 100))
summary(outdeg2)
quantile(outdeg2, probs = c(2.5 / 100, 97.5 / 100))
summary(outdeg3)
quantile(outdeg3, probs = c(2.5 / 100, 97.5 / 100))
# summary(outdeg4); quantile(outdeg4, probs = c(2.5/100, 97.5/100))

outdeg.gr.0.3 <- n - (outdeg0 + outdeg1 + outdeg2 + outdeg3)
summary(outdeg.gr.0.3)
quantile(outdeg.gr.0.3, probs = c(2.5 / 100, 97.5 / 100))


### target
target_stats_outdeg



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
### simulated
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

#quantile(unlist(lapply(sim.race.num, function(x) x["mix.race.num.1.1"])), probs = c(2.5 / 100, 97.5 / 100))
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

### target
target_race_mixing <- 
    c(target.w.w, target.b.w, target.h.w, target.o.w,
      target.w.b, target.b.b, target.h.b, target.o.b,
      target.w.h, target.b.h, target.h.h, target.o.h,
      target.o.w, target.o.b, target.o.h, target.o.w)

## gender

### simulated
sim.gender <- lapply(nsim.vec, function(x) summary(sim_results[[x]] ~ nodemix("gender")))
summary(unlist(lapply(sim.gender, function(x) x["mix.gender.female.female"])))
summary(unlist(lapply(sim.gender, function(x) x["mix.gender.male.female"])))
summary(unlist(lapply(sim.gender, function(x) x["mix.gender.female.male"])))
summary(unlist(lapply(sim.gender, function(x) x["mix.gender.male.male"])))

quantile(unlist(lapply(sim.gender, function(x) x["mix.gender.female.female"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.gender, function(x) x["mix.gender.male.female"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.gender, function(x) x["mix.gender.female.male"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.gender, function(x) x["mix.gender.male.male"])), probs = c(2.5 / 100, 97.5 / 100))

### 
target_gender_mixing <- 
    c(tgt.female.pctmale, tgt.male.pctfemale, tgt.male.pctmale)        


## age
### simulated
sim.young <- lapply(nsim.vec, function(x) summary(sim_results[[x]] ~ nodemix("young")))
summary(unlist(lapply(sim.young, function(x) x["mix.young.0.0"])))
summary(unlist(lapply(sim.young, function(x) x["mix.young.1.0"])))
summary(unlist(lapply(sim.young, function(x) x["mix.young.0.1"])))
summary(unlist(lapply(sim.young, function(x) x["mix.young.1.1"])))

quantile(unlist(lapply(sim.young, function(x) x["mix.young.0.0"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.young, function(x) x["mix.young.1.0"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.young, function(x) x["mix.young.0.1"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.young, function(x) x["mix.young.1.1"])), probs = c(2.5 / 100, 97.5 / 100))

### target
target_age_mixing <- c(tgt.old.pctyoung, tgt.young.pctold, tgt.young.pctyoung)


## distance
### simulated
sim.dist <- lapply(nsim.vec, function(x) summary(sim_results[[x]] ~ dist(1:4)))
summary(unlist(lapply(sim.dist, function(x) x["dist1"])))
summary(unlist(lapply(sim.dist, function(x) x["dist2"])))
summary(unlist(lapply(sim.dist, function(x) x["dist3"])))
summary(unlist(lapply(sim.dist, function(x) x["dist4"])))

quantile(unlist(lapply(sim.dist, function(x) x["dist1"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.dist, function(x) x["dist2"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.dist, function(x) x["dist3"])), probs = c(2.5 / 100, 97.5 / 100))
quantile(unlist(lapply(sim.dist, function(x) x["dist4"])), probs = c(2.5 / 100, 97.5 / 100))

### target
target_distance <- c(dist.nedge.distribution[dist.terms])


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

ggsave(here("simulate-from-ergms", "out", "edges_violin_plot.png"), width = 8, height = 6)

## outdegree
outdeg_df <- data.frame(
  outdegree = c(outdeg0, outdeg1, outdeg2, outdeg3),
  category = rep(c("outdeg0", "outdeg1", "outdeg2", "outdeg3"),
    times = c(length(outdeg0), length(outdeg1), length(outdeg2), length(outdeg3))
  )
)

target_values <- target_stats_outdeg

ggplot(outdeg_df, aes(x = category, y = outdegree)) +
  geom_violin(trim = FALSE, fill = "#66C2A5") +
  geom_hline(
    data = data.frame(
      category = c("outdeg0", "outdeg1", "outdeg2", "outdeg3"),
      y = target_values
    ), aes(yintercept = y),
    linetype = "solid", color = "black", size = 1.5
  ) +
  facet_wrap(~category, scales = "free_y") +
  theme_minimal() +
  labs(y = "outdegree", x = "") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 14, face = "bold")
  )
  ggsave(here("simulate-from-ergms", "out", "outdeg_violin_plot.png"), width = 8, height = 6)


## indegree

target_values <- target_stats_indeg[1:2]

# Manually create a new data frame
indeg_df_clean <- data.frame(
  indegree = c(indeg0, indeg1),
  category = rep(c("indeg0", "indeg1"), each = length(indeg0))
)

# Plot the clean data frame
# Create the plot with target lines
ggplot(indeg_df_clean, aes(x = category, y = indegree)) +
  geom_violin(trim = FALSE, fill = "#66C2A5") +
  geom_hline(
    data = data.frame(
      category = c("indeg0", "indeg1"),
      y = target_values
    ), aes(yintercept = y),
    linetype = "solid", color = "black", size = 1.5
  ) +
  facet_wrap(~category, scales = "free_y", nrow = 1, ncol = 2) +
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

## race
race_mixing_df <- do.call(rbind, lapply(sim.race.num, as.data.frame))
race_mixing_df <- stack(race_mixing_df)

# Rename the columns to more intuitive names
colnames(race_mixing_df) <- c("count", "category")

# Plot the violin plots with faceting
ggplot(race_mixing_df, aes(x = category, y = count)) +
  geom_violin(trim = FALSE, fill = "#66C2A5") +
  geom_hline(data = data.frame(category = unique(race_mixing_df$category), 
                               y = target_race_mixing), aes(yintercept = y), 
             linetype = "solid", color = "red", linewidth = 1.5) +
  facet_wrap(~ category, scales = "free_y") +
  theme_minimal() +
  labs(y = "Race Mixing Count", x = NULL) +
  theme(
    axis.text.x = element_blank(),  # Hide x-axis text
    axis.title.x = element_blank(),  # Hide x-axis title
    axis.title.y = element_text(size = 14),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 14, face = "bold")  # Make panel titles more prominent
  )

  ggsave(here("simulate-from-ergms", "out", "racemix_violin_plot.png"), width = 8, height = 6)

## gender



## age



## distance
