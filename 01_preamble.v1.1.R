#!/usr/bin/env Rscript

## ---------------------------
##
## Script name:  01_preamble.v1.1.R
##
## Purpose of script: Analyse ParkProReakt study results (2022–2025), including
##                    PDQ-39, BDI, MoCA, UPDRS, Hoehn & Yahr, demographics,
##                    and additional questionnaires.
##
## Authors: Antonia Koelble, David Pedrosa
##
## ---------------------------
##
## Notes:
##   - Project: ParkProReakt (2022–2025)
##   - GitHub repository: https://github.com/dpedrosac/flagshipPPR/
##
## ---------------------------
##
## Version history:
##   1.3 — 2025-05-12 — Sorted the codebase and added new functionality.
##   1.2 — 2025-12-11 — Major restructuring; removed redundancies and reorganized logic.
##
## ---------------------------


################################################################################
# Package management
################################################################################

# Define required packages in a single vector
pkgs <- c(
  "consort", "dplyr", "emmeans", "ggplot2", "janitor", "lme4", "lubridate",
  "patchwork", "purrr", "readr", "survival", "sjPlot", "stringr", "tableone",
  "tidyr", "tidyverse"
)

# Install any packages that are not yet installed and load them
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}

################################################################################
# Paths and global settings
################################################################################

# Toggle this to TRUE when you want to see quick checks in the console/Viewer
sanity_check <- FALSE

# Get user name (works on Windows and almost all other systems)
user_name <- Sys.getenv("USERNAME", unset = Sys.info()[["user"]])

# Define base directory depending on user
if (user_name == "akoel") {
  base_dir <- file.path("C:", "Users", "akoel", "OneDrive")
} else if (user_name == "david") {
  base_dir <- file.path("/", "media", "storage", "flagshipPPR")
} else {
  # Fallback: use the user's home directory
  base_dir <- path.expand("~")
  message(
    "⚠️ Unknown user: ", user_name,
    ". Using default home directory instead."
  )
}

# Define path where results are saved
# (this folder will be created if it doesn't exist)
results_dir <- file.path(base_dir, "results")

if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
  message("📁 Created folder: ", results_dir)
}


################################################################################
# Data preparation
################################################################################

# Source data-cleaning script (creates sorted_pdq39 etc.)
# NOTE: consider renaming this file to `02_cleanup_data.R` and this one to
#       `01_settings.R` or similar to reflect responsibilities more clearly.
#source("02_cleanup_data.v1.3.R")


############################################################################################
# Load data data into workspace:
############################################################################################

sorted_pdq39 <- readr::read_csv( # Already sorted PDQ39 data
  file.path(results_dir, "sorted_pdq39.csv"),
  show_col_types = FALSE
)


#TODO: I would put the rest in a distinct file called statistics and probably rename this one
# to settings.R or something similar. 

############################################################################################
# Descriptive statistics:
############################################################################################

analysis_both <- sorted_pdq39 %>%
  dplyr::group_by(group, Timepoint) %>%           # <- group by both
  dplyr::summarise(
    n        = sum(!is.na(pdq39_sum_index)),
    Mean     = mean(pdq39_sum_index, na.rm = TRUE),
    Median   = median(pdq39_sum_index, na.rm = TRUE),
    SD       = stats::sd(pdq39_sum_index, na.rm = TRUE),
    SE       = SD / sqrt(n),
    Minimum  = min(pdq39_sum_index, na.rm = TRUE),
    Q1       = stats::quantile(pdq39_sum_index, 0.25, na.rm = TRUE, names = FALSE),
    IQR      = stats::IQR(pdq39_sum_index, na.rm = TRUE),
    Q3       = stats::quantile(pdq39_sum_index, 0.75, na.rm = TRUE, names = FALSE),
    Maximum  = max(pdq39_sum_index, na.rm = TRUE),
    Variance = stats::var(pdq39_sum_index, na.rm = TRUE),
    lower    = Q1 - 1.5 * IQR,   # Tukey lower fence
    upper    = Q3 + 1.5 * IQR,   # Tukey upper fence
    .groups  = "drop"
  )

# Only show outputs if sanity_check is TRUE
if (isTRUE(sanity_check)) {
  print(analysis_both)                # console preview
  if (interactive()) utils::View(analysis_both, title = "Intervention descriptives")
}

############################################################################################
# Boxplots, group- and time-sorted:
############################################################################################


pdq39_T0_T6 <- sorted_pdq39 %>% filter(Timepoint %in% c("T0", "T6"))

# example for boxplots of two groups (as facets)
boxplot_pdq39 <- ggplot(
  data = pdq39_T0_T6,
  aes(x = Timepoint, y = pdq39_sum_index, fill = Timepoint)
) +
  geom_boxplot(outlier.colour = "red", outlier.size = 1.8, width = 0.5) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
  facet_wrap(~ group) +  # Creates two facets: one per group
  labs(
    title = "PDQ-39 Scores by Timepoint and Group",
    subtitle = "Comparison between Intervention and Control groups",
    x = "Timepoint",
    y = "PDQ-39 Sum Index"
  ) +
  scale_fill_brewer(palette = "Blues") +  # nicer but subtle palette
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",              # remove redundant legend
    strip.text = element_text(face = "bold"),  # emphasize facet labels
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),    # cleaner grid
    panel.grid.major.x = element_blank()
  )

# example of the boxplots per Timepoint:
boxplot_pdq39_groupside <- ggplot(
  data = pdq39_T0_T6,
  aes(x = Timepoint, y = pdq39_sum_index, fill = group)
) +
  geom_boxplot(
    position = position_dodge(width = 0.8), # side-by-side boxes
    outlier.colour = "red",
    outlier.size = 1.8,
    width = 0.6
  ) +
  geom_jitter(
    aes(color = group),
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
    alpha = 0.4,
    size = 1.5
  ) +
  labs(
    title = "PDQ-39 Scores by Timepoint and Group",
    x = "Timepoint",
    y = "PDQ-39 Sum Index",
    fill = "Group",
    color = "Group"
  ) +
  scale_fill_brewer(palette = "Blues") +
  scale_color_brewer(palette = "Blues") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )


# TODO: Not sure what you wanted to save, so I commented it out
# Filename within that folder to save
# plot_path <- file.path(results_dir, "boxplot_combined.png")

# Save the plot (e.g., the improved boxplot_intervention or boxplot_pdq39_groupside)
#ggsave(
#  filename = plot_path,
#  plot     = boxplot_intervention,   # or whatever your plot object is called
#  width    = 8,
#  height   = 6,
#  dpi      = 300
#)

############################################################################################
# Main analysis:
############################################################################################

# Plot Mean +/- SD for T0 and T6
time_levels <- paste0("T", 0:6)

sorted_pdq39 <- sorted_pdq39 %>%
  mutate(Timepoint = factor(Timepoint, levels = time_levels))

# Summary statistics
df_summary <- pdq39_T0_T6 %>%
  group_by(group, Timepoint) %>%
  summarise(
    n    = sum(!is.na(pdq39_sum_index)),
    mean = mean(pdq39_sum_index, na.rm = TRUE),
    sd   = sd(pdq39_sum_index, na.rm = TRUE),
    se   = sd / sqrt(n),
    .groups = "drop"
  )

# Start plotting
dodge_w <- 0.7
pdodge  <- position_dodge(width = dodge_w)

mean_sd <- ggplot(df_summary, aes(x = Timepoint, y = mean, color = group, group = group)) +
  # mean ± 2SD error bars
  geom_errorbar(
    aes(ymin = mean - 2 * sd, ymax = mean + 2 * sd),
    width = 0.15, position = pdodge, linewidth = 0.7
  ) +
  # mean points
  geom_point(position = pdodge, size = 3) +
  # connecting lines (optional)
  geom_line(
    aes(group = group),
    position = pdodge, color = "grey50", linewidth = 0.6
  ) +
  labs(
    title = "PDQ-39 Mean ± 2SD by Timepoint and Group",
    x = "Timepoint",
    y = "Mean PDQ-39 (± 2 SD)",
    color = "Group"
  ) +
  scale_color_manual(
    values = c("Control" = "#1b9e77", "Intervention" = "#d95f02")
  ) +
  scale_x_discrete(drop = FALSE, limits = time_levels) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(size = 15),
    axis.text  = element_text(size = 13),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Show only when checking
if (isTRUE(sanity_check)) print(mean_sd)

# Plot Mean +/- SE for T0 and T6
pdodge  <- position_dodge(width = dodge_w) # not necessary,I think

mean_se <- ggplot(df_summary, aes(x = Timepoint, y = mean, color = group, group = group)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.15, position = pdodge, linewidth = 0.7) +
  geom_point(position = pdodge, size = 3) +
  # optional: light connector to aid reading (kept grey so color encodes points)
  geom_line(aes(group = group), position = pdodge, color = "grey55", linewidth = 0.6) +
  scale_x_discrete(drop = FALSE, limits = time_levels) +
  scale_color_manual(values = c("Control" = "#1b9e77", "Intervention" = "#d95f02")) +
  labs(
    title = "PDQ-39 Mittelwert ± SE (Intervention vs. Kontrolle)",
    x = "Zeitpunkt",
    y = "Mittelwert PDQ-39 (± SE)",
    color = "Gruppe"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Show only when checking
if (isTRUE(sanity_check)) print(mean_se)


## Proposed version of a plot over time with the option to look for outliers:

# sorted_pdq39$Timepoint <- factor(sorted_pdq39$Timepoint, levels = c("T0","T1","T2","T3","T4","T5","T6","T7"))
sorted_pdq39 <- sorted_pdq39 %>% # a bit less manual text
  dplyr::mutate(
    Timepoint = factor(Timepoint, levels = paste0("T", 0:7))
  )

time_levels <- paste0("T", 0:6) # drop T7

# Make sure your data uses these levels (keeps empty timepoints on the axis if needed)
sorted_pdq39 <- sorted_pdq39 %>%
  mutate(Timepoint = factor(Timepoint, levels = time_levels))

# (Re)build analysis_both so it only contains T0..T6 (optional if already done)
analysis_both <- analysis_both %>%
  filter(Timepoint %in% time_levels) %>%
  mutate(Timepoint = factor(Timepoint, levels = time_levels)) %>%
  complete(group, Timepoint)

# 1) Outlier flags for jitter (unchanged logic)
points_pdq <- sorted_pdq39 %>%
  left_join(analysis_both %>% select(group, Timepoint, lower, upper),
            by = c("group", "Timepoint")) %>%
  mutate(is_outlier = pdq39_sum_index < lower | pdq39_sum_index > upper)

# 2) Use ONE dodge width everywhere to align means, SE bars, and jitter
dodge_w <- 0.7

# Align means/SE to jitter: use position_dodge (not position_dodge2)
pdodge_means <- position_dodge(width = dodge_w)

p_err <- ggplot(analysis_both, aes(x = Timepoint, y = Mean, group = group, color = group)) +
  geom_errorbar(
    aes(ymin = Mean - SE, ymax = Mean + SE),
    position = pdodge_means, width = 0.15, linewidth = 0.7
  ) +
  geom_point(position = pdodge_means, size = 2.8) +
  geom_jitter(
    data = points_pdq,
    aes(y = pdq39_sum_index, color = ifelse(is_outlier, "Outlier", "Inlier")),
    position = position_jitterdodge(jitter.width = 0.10, dodge.width = dodge_w),
    alpha = 0.7, size = 1.9, show.legend = TRUE
  ) +
  scale_x_discrete(drop = FALSE, limits = time_levels) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Inlier" = "grey60",
      "Outlier" = "red",
      "Control" = "#1b9e77",
      "Intervention" = "#d95f02"
    )
  ) +
  labs(
    title = "PDQ-39 Mean ± SE by Timepoint and Group",
    x = "Timepoint", y = "PDQ-39 Sum Index"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# 3) Mean-connecting segments across timepoints, trimmed so they don't touch points
trim_x <- 0.02  # how much to shorten at each end along x

# Group order (left/right order in the dodge). Set explicitly if you like:
grp_lvls <- levels(factor(analysis_both$group))  # e.g., c("Control","Intervention")
offsets  <- seq(-dodge_w/2, dodge_w/2, length.out = length(grp_lvls))

means_pos <- analysis_both %>%
  mutate(
    Timepoint = factor(Timepoint, levels = time_levels),
    g_idx     = as.integer(factor(group, levels = grp_lvls)),
    x_base    = as.numeric(Timepoint),
    x_pos     = x_base + offsets[g_idx]
  ) %>%
  arrange(group, Timepoint)

seg_df <- means_pos %>%
  group_by(group) %>%
  arrange(Timepoint, .by_group = TRUE) %>%
  mutate(
    x_next = lead(x_pos),
    y_next = lead(Mean)
  ) %>%
  filter(!is.na(x_next), !is.na(y_next)) %>%
  mutate(
    x_draw    = x_pos  + trim_x,
    xend_draw = x_next - trim_x
  ) %>%
  ungroup()

p_err <- p_err +
  geom_segment(
    data = seg_df,
    aes(x = x_draw, xend = xend_draw, y = Mean, yend = y_next),
    inherit.aes = FALSE,
    color = "grey55",
    linewidth = 0.8
  )
