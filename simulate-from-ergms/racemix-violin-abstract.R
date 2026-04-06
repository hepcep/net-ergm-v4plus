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
# White->White is included but flagged as unspecified (reference category)
race_order  <- c("White", "Black", "Hispanic", "Other")
level_order <- apply(expand.grid(sender = race_order, receiver = race_order), 1,
                     function(r) paste0(r["sender"], " -> ", r["receiver"]))

race_mixing_df$label    <- factor(race_mixing_df$label, levels = level_order)
race_mixing_df$specified <- TRUE


# Add White->White (reference category, not a model term) ---------------------

ww_sim <- sapply(seq_along(sim_results), function(i)
  network.edgecount(sim_results[[i]]) - sum(sim.race.num[[i]]))

ww_df <- data.frame(
  count    = ww_sim,
  category = "mix.race.num.1.1",
  sender   = "White",
  receiver = "White",
  label    = factor("White -> White", levels = level_order),
  specified = FALSE
)

race_mixing_df <- dplyr::bind_rows(race_mixing_df, ww_df)


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

# Add White->White target
target_df <- dplyr::bind_rows(
  target_df,
  data.frame(
    category = "target.w.w",
    y        = edges_target - sum(target_race_num),
    sender   = "White",
    receiver = "White",
    label    = factor("White -> White", levels = level_order)
  )
)


# Plot -------------------------------------------------------------------------

ggplot(race_mixing_df, aes(x = label, y = count, fill = specified)) +
  geom_violin(trim = FALSE, alpha = 0.75, color = NA) +
geom_hline(
    data    = target_df,
    mapping = aes(yintercept = y),
    linetype = "dashed", color = "#E15759", linewidth = 0.9
  ) +
  facet_wrap(~ label, scales = "free", ncol = 4) +
  scale_fill_manual(
    values = c("TRUE" = "#4E79A7", "FALSE" = "#B8B0AC"),
    labels = c("TRUE" = "Model term", "FALSE" = "Reference (unspecified)"),
    name   = NULL
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 13) +
  labs(
    y       = "Edge Count",
    x       = NULL,
    caption = NULL
  ) +
  theme(
    axis.text.x      = element_blank(),
    axis.ticks.x     = element_blank(),
    axis.text.y      = element_text(size = 10),
    axis.title.y     = element_text(size = 13),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(size = 11, face = "bold"),
    panel.spacing    = unit(1, "lines"),
    legend.position  = "bottom",
    legend.text      = element_text(size = 11)
  )

ggsave(
  here("simulate-from-ergms", "out", "racemix_violin_abstract.png"),
  width = 14, height = 12, dpi = 300
)


# Supplemental plot: normalized deviations from target (single panel) ---------

# Join target values into the simulated data and compute % deviation
race_mixing_norm <- race_mixing_df |>
  dplyr::left_join(
    target_df |> dplyr::select(label, target = y),
    by = "label"
  ) |>
  dplyr::mutate(pct_dev = (count - target) / target * 100)

ggplot(race_mixing_norm, aes(x = label, y = pct_dev)) +
  geom_violin(trim = FALSE, fill = "#4E79A7", alpha = 0.75, color = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#E15759", linewidth = 0.9) +
  coord_flip() +
  theme_minimal(base_size = 11) +
  labs(
    x       = NULL,
    y       = "% deviation from target",
    caption = "Dashed line = target; violin = distribution across 100 simulated networks"
  ) +
  theme(
    axis.text.y      = element_text(size = 9),
    axis.title.x     = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 8, color = "grey50")
  )

ggsave(
  here("simulate-from-ergms", "out", "racemix_violin_abstract_supp.png"),
  width = 7, height = 8, dpi = 300
)
