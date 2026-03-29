# Race mixing violin plot — polished version for abstract
# Data prep mirrors summaries-across-simulated-distributions.R

rm(list = ls())

library(renv)
renv::activate()

library(network)
library(ergm)
library(dplyr)
library(ergm.userterms.hepcep)
library(here)
library(ggplot2)
library(qs)


# Data -------------------------------------------------------------------------

run_label <- "new-mixing-data-w-dnf"

sim_results <- qread(here("simulate-from-ergms", "out",
                          paste0(run_label, "_sim_results_10.qs")))
                          ## swap to _100.qs for full run

data_objects <- readRDS(here("fit-ergms", "out", "processed_data.rds"))
list2env(data_objects, envir = globalenv())


# Compute race mixing ----------------------------------------------------------

sim.race.num <- lapply(seq_along(sim_results), function(x)
  summary(sim_results[[x]] ~ nodemix("race.num")))


# Build long data frame --------------------------------------------------------

race_mixing_df <- do.call(rbind, lapply(seq_along(sim.race.num), function(i) {
  x <- sim.race.num[[i]]
  data.frame(count = as.numeric(x), category = names(x), stringsAsFactors = FALSE)
}))


# Map numeric codes to readable sender→receiver labels ------------------------

race_num  <- c("1" = "White", "2" = "Black", "3" = "Hispanic", "4" = "Other")
race_abbr <- c("w"  = "White", "b"  = "Black", "h"  = "Hispanic", "o"  = "Other")

race_mixing_df <- race_mixing_df |>
  dplyr::mutate(
    sender   = race_num[sub("mix\\.race\\.num\\.(\\d)\\.(\\d)", "\\1", category)],
    receiver = race_num[sub("mix\\.race\\.num\\.(\\d)\\.(\\d)", "\\2", category)],
    label    = paste0(sender, " -> ", receiver)   # e.g. "Black -> White"
  )

# Order panels as a near-matrix: vary receiver across columns, sender down rows
# (White→White is the omitted reference category)
race_order  <- c("White", "Black", "Hispanic", "Other")
level_order <- apply(expand.grid(sender = race_order, receiver = race_order), 1,
                     function(r) paste0(r["sender"], " -> ", r["receiver"]))
level_order <- setdiff(level_order, "White -> White")

race_mixing_df$label <- factor(race_mixing_df$label, levels = level_order)


# Target values ----------------------------------------------------------------

target_df <- data.frame(
  category = names(target_race_num),
  y        = as.numeric(target_race_num)
) |>
  dplyr::mutate(
    sender   = race_abbr[sub("target\\.(\\w)\\.(\\w)", "\\1", category)],
    receiver = race_abbr[sub("target\\.(\\w)\\.(\\w)", "\\2", category)],
    label    = factor(paste0(sender, " -> ", receiver), levels = level_order)
  )


# Plot -------------------------------------------------------------------------

ggplot(race_mixing_df, aes(x = label, y = count)) +
  geom_violin(trim = FALSE, fill = "#4E79A7", alpha = 0.75, color = NA) +
  geom_hline(
    data    = target_df,
    mapping = aes(yintercept = y),
    linetype = "dashed", color = "#E15759", linewidth = 0.9
  ) +
  facet_wrap(~ label, scales = "free_y", ncol = 4) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 11) +
  labs(
    y       = "Edge Count",
    x       = NULL,
    caption = "Dashed line = target; violin = distribution across 100 simulated networks"
  ) +
  theme(
    axis.text.x      = element_blank(),
    axis.ticks.x     = element_blank(),
    axis.title.y     = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(size = 9, face = "bold"),
    plot.caption     = element_text(size = 8, color = "grey50"),
    panel.spacing    = unit(0.8, "lines")
  )

ggsave(
  here("simulate-from-ergms", "out", "racemix_violin_abstract.png"),
  width = 10, height = 8, dpi = 300
)
