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
library(qs)
library(knitr)
library(rmarkdown)


# Data ----------

run_label <- "stepwise-refactored-checkpointing-data-dated-2025-jan23-redone" # set manually to ensure intentional updates
## should match the object from the ERGM fitting code 

  ## load data
  sim_results <- qread(here("simulate-from-ergms", "out", paste0(run_label, "_sim_results_10.qs")))

## input params
  data_objects <- readRDS(here("fit-ergms", "out", "processed_data.rds"))
  names(data_objects)

## confirm which run
  run_label

## Unpack objects in data_objects
intersect(names(data_objects), ls())
list2env(data_objects, envir = globalenv())
ls()


# Compute summaries and IQRs ----------

n <- network.size(sim_results[[1]])
n_edges <- edges_target


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
  ## target
  target_stats_outdeg <- outdegree_data$mean_n[1:2] #target
  target_stats_outdeg

  ## simulated
  outdeg0 <- unlist(lapply(sim_results, 
                          function (x) summary(x ~ odegree(0))
                          ))
  outdeg1 <- unlist(lapply(sim_results, 
                          function (x) summary(x ~ odegree(1))))
  #
  summary(outdeg0)
  quantile(outdeg0, probs = c(2.5 / 100, 97.5 / 100))
  summary(outdeg1)
  quantile(outdeg1, probs = c(2.5 / 100, 97.5 / 100))

  outdeg.gr.0.1 <- n - (sum(target_stats_outdeg))
  summary(outdeg.gr.0.1)
  quantile(outdeg.gr.0.1, probs = c(2.5 / 100, 97.5 / 100))

## indegree
  ## target
  target_stats_indeg <- indegree_data$mean_n[1:2]


  ## simulated
  indeg0 <- unlist(lapply(sim_results, 
                         function (x) summary(x ~ idegree(0))
  ))

  indeg1 <- unlist(lapply(sim_results, 
                          function (x) summary(x ~ idegree(1))
    ))

  summary(indeg0)
  quantile(indeg0, probs = c(2.5 / 100, 97.5 / 100))
  summary(indeg1)
  quantile(indeg1, probs = c(2.5 / 100, 97.5 / 100))

  indeg.gr.0.1 <- n - (sum(target_stats_indeg))
  summary(indeg.gr.0.1)
  quantile(indeg.gr.0.1, probs = c(2.5 / 100, 97.5 / 100))

  ### target
  target_stats_indeg

## race
  ### simulated
  sim.race.num <- lapply(1:length(sim_results), 
    function(x) summary(sim_results[[x]] ~ nodemix("race.num")))
  #summary(unlist(lapply(sim.race.num, function(x) x["mix.race.num.1.1"])))
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
target_race_mixing <- target_race_num

## sex
  ### simulated
sim.sex <- lapply(sim_results, function(x) {
    s <- summary(x ~ nodemix("sex"))})
  #summary(unlist(lapply(sim.sex, function(x) x["mix.sex.F.F"])))
  summary(unlist(lapply(sim.sex, function(x) x["mix.sex.M.F"])))
  summary(unlist(lapply(sim.sex, function(x) x["mix.sex.F.M"])))
  summary(unlist(lapply(sim.sex, function(x) x["mix.sex.M.M"])))

  #quantile(unlist(lapply(sim.sex, function(x) x["mix.sex.F.F"])), probs = c(2.5 / 100, 97.5 / 100))
  quantile(unlist(lapply(sim.sex, function(x) x["mix.sex.M.F"])), probs = c(2.5 / 100, 97.5 / 100))
  quantile(unlist(lapply(sim.sex, function(x) x["mix.sex.F.M"])), probs = c(2.5 / 100, 97.5 / 100))
  quantile(unlist(lapply(sim.sex, function(x) x["mix.sex.M.M"])), probs = c(2.5 / 100, 97.5 / 100))

  ### 
  target_sex_mixing <- c(tgt.female.pctmale, tgt.male.pctfemale, tgt.male.pctmale)

## age
  ### simulated
  sim.young <- lapply(sim_results, function(x) {
    s <- summary(x ~ nodemix("young"))})

  #summary(unlist(lapply(sim.young, function(x) x["mix.young.0.0"])))
  summary(unlist(lapply(sim.young, function(x) x["mix.young.1.0"])))
  summary(unlist(lapply(sim.young, function(x) x["mix.young.0.1"])))
  summary(unlist(lapply(sim.young, function(x) x["mix.young.1.1"])))

  #quantile(unlist(lapply(sim.young, function(x) x["mix.young.0.0"])), probs = c(2.5 / 100, 97.5 / 100))
  quantile(unlist(lapply(sim.young, function(x) x["mix.young.1.0"])), probs = c(2.5 / 100, 97.5 / 100))
  quantile(unlist(lapply(sim.young, function(x) x["mix.young.0.1"])), probs = c(2.5 / 100, 97.5 / 100))
  quantile(unlist(lapply(sim.young, function(x) x["mix.young.1.1"])), probs = c(2.5 / 100, 97.5 / 100))

  ### target
  target_age_mixing <- c(
  tgt.young.pctold,   # young → old
  tgt.old.pctyoung,   # old → young
  tgt.young.pctyoung  # young → young
)
names(target_age_mixing) <- c("mix.young.1.0", "mix.young.0.1", "mix.young.1.1")

## distance
  ### simulated
  sim.dist <- lapply(1:length(sim_results), 
      function(x) summary(sim_results[[x]] ~ dist(1:4)))
  summary(unlist(lapply(sim.dist, function(x) x["dist1"])))
  summary(unlist(lapply(sim.dist, function(x) x["dist2"])))
  summary(unlist(lapply(sim.dist, function(x) x["dist3"])))
  #summary(unlist(lapply(sim.dist, function(x) x["dist4"])))

  quantile(unlist(lapply(sim.dist, function(x) x["dist1"])), probs = c(2.5 / 100, 97.5 / 100))
  quantile(unlist(lapply(sim.dist, function(x) x["dist2"])), probs = c(2.5 / 100, 97.5 / 100))
  quantile(unlist(lapply(sim.dist, function(x) x["dist3"])), probs = c(2.5 / 100, 97.5 / 100))
  #quantile(unlist(lapply(sim.dist, function(x) x["dist4"])), probs = c(2.5 / 100, 97.5 / 100))

  ### target
  target_distance <- dist_nedge_distribution
  target_distance <- target_distance[1:3] #hard coded that 4th term is left out


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
    outdegree = 
    c(outdeg0, outdeg1),
    category = rep(c("outdeg0", "outdeg1"), each = c(length(outdeg0), length(outdeg1))
      )    
      )


  target_values <- target_stats_outdeg

  ggplot(outdeg_df, aes(x = category, y = outdegree)) +
    geom_violin(trim = FALSE, fill = "#66C2A5") +
    geom_hline(
      data = data.frame(
        category = c("outdeg0", "outdeg1"),
        y = target_values
      ),
      mapping = aes(yintercept = y, category = category),
      linetype = "solid", color = "black", size = 1.5
    )+
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
  ## create an empty data frame
  race_mixing_df <- data.frame()

  ## loop through sim.race.num and extract data into a proper format
  for (i in seq_along(sim.race.num)) {
    temp_df <- as.data.frame(t(sim.race.num[[i]]))  # Transpose to get each category in columns
    temp_df <- stack(temp_df)  # Stack to get a long format
    race_mixing_df <- rbind(race_mixing_df, temp_df)  # Bind to the main data frame
  }

  str(race_mixing_df)

  # Rename the columns
  colnames(race_mixing_df) <- c("count", "category")

  # convert category to factor
  race_mixing_df$category <- factor(race_mixing_df$category)

  # Remove the first target value
  #target_race_mixing_filtered <- target_race_mixing[-1]
  target_race_mixing_filtered <- target_race_mixing

  # plot the data
  ggplot(race_mixing_df, aes(x = category, y = count)) +
    geom_violin(trim = FALSE, fill = "#66C2A5") +
    geom_hline(data = data.frame(category = unique(race_mixing_df$category), 
                                y = target_race_mixing_filtered), aes(yintercept = y), 
              linetype = "solid", color = "black", linewidth = 1.5) +
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



## sex
  sex_mixing_df <- do.call(rbind, lapply(seq_along(sim.sex), function(i) {
  data.frame(
    run = i,
    category = names(sim.sex[[i]]),
    count = as.numeric(sim.sex[[i]])
  )
}))


  sex_mixing_df$category <- factor(sex_mixing_df$category)
  head(sex_mixing_df)

  target_sex_mixing
  names(target_sex_mixing) <- c("mix.sex.M.F",   "mix.sex.F.M",  "mix.sex.M.M")

  ggplot(sex_mixing_df, aes(x = category, y = count)) +
    geom_violin(trim = FALSE, fill = "#66C2A5") +
    geom_hline(data = data.frame(category = names(target_sex_mixing), 
                                y = as.numeric(target_sex_mixing)), aes(yintercept = y), 
              linetype = "solid", color = "black", linewidth = 1.5) +
    facet_wrap(~ category, scales = "free_y") +
    theme_minimal() +
    labs(y = "Sex Mixing Count", x = NULL) +
    theme(
      axis.text.x = element_blank(),  # Hide x-axis text
      axis.title.x = element_blank(),  # Hide x-axis title
      axis.title.y = element_text(size = 14),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 14, face = "bold")  # Make panel titles more prominent
    )

      ggsave(here("simulate-from-ergms", "out", "sexmix_violin_plot.png"), width = 8, height = 6)

  
  #stop("\n", "!!!RUN UP TO HERE!!!", "\n")

  ## age
    # Combine the list elements into a data frame, excluding the base category
    age_mixing_df <- do.call(rbind, lapply(seq_along(sim.young), function(i) {
      data.frame(
        run = i,
        category = names(sim.young[[i]]),
        count = as.numeric(sim.young[[i]])
      )
    }))

    # Filter out the base category
    age_mixing_df <- age_mixing_df[!age_mixing_df$category %in% "mix.young.0.0", ]
    levels(age_mixing_df$category)

    # Define desired panel order explicitly
    desired_levels <- c("mix.young.0.1", "mix.young.1.0", "mix.young.1.1")

    # Set factor levels to desired order before plotting or joining
    age_mixing_df$category <- factor(age_mixing_df$category, levels = desired_levels)

    # Create target values (from unpacked objects earlier)
    names(target_age_mixing) <- desired_levels

    # Create a target values dataframe
    target_age_df <- data.frame(
      category = names(target_age_mixing),
      target = as.numeric(target_age_mixing)
    )

    # Apply same factor levels to target dataframe
    target_age_df$category <- factor(target_age_df$category, levels = desired_levels)

    # Merge target into main dataframe
    age_mixing_df <- dplyr::left_join(age_mixing_df, target_age_df, by = "category")

    # Plot
    ggplot(age_mixing_df, aes(x = category, y = count)) +
      geom_violin(trim = FALSE, fill = "#66C2A5") +
      geom_hline(aes(yintercept = target), linetype = "solid", color = "black", linewidth = 1.5) +
      facet_wrap(~ category, scales = "free_y") +
      theme_minimal() +
      labs(y = "Age Mixing Count", x = NULL) +
      theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 14, face = "bold")
      )

    # Save the plot
    ggsave(here("simulate-from-ergms", "out", "agemix_violin_plot.png"), width = 8, height = 6)



  ## distance
    distance_mixing_df <- do.call(rbind, lapply(seq_along(sim.dist), function(i) {
      data.frame(
        run = i,
        category = names(sim.dist[[i]]),
        count = as.numeric(sim.dist[[i]])
      )
    }))

    distance_mixing_df$category <- factor(distance_mixing_df$category)

    distance_mixing_df_filtered <- distance_mixing_df[distance_mixing_df$category != "dist4", ]

    names(target_distance) <- c("dist1", "dist2", "dist3")

    # Plot the violin plot excluding dist4
    ggplot(distance_mixing_df_filtered, aes(x = category, y = count)) +
      geom_violin(trim = FALSE, fill = "#66C2A5") +
      geom_hline(data = data.frame(category = names(target_distance), 
                                  y = as.numeric(target_distance)), aes(yintercept = y), 
                linetype = "solid", color = "black", linewidth = 1.5) +
      facet_wrap(~ category, scales = "free_y") +
      theme_minimal() +
      labs(y = "Distance Metric Count", x = NULL) +
      theme(
        axis.text.x = element_blank(),  # Hide x-axis text
        axis.title.x = element_blank(),  # Hide x-axis title
        axis.title.y = element_text(size = 14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 14, face = "bold")  # Make panel titles more prominent
      )

    # Save the plot
    ggsave(here("simulate-from-ergms", "out", "distance_violin_plot.png"), width = 8, height = 6)




