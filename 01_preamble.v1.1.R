#!/usr/bin/env Rscript

# ---------------------------
# Script name:  01_preamble.v1.1.R
# Purpose: 	Bookkeeping of scripts, Load data, search outliers in primary 
# 		outcome
#
# Authors: 	Antonia Koelble, Anna Pedrosa, Hanna Fischer, David Pedrosa
#
# ---------------------------
#
# Notes:
# Project:     ParkProReakt (2022–2025)
# Repository:  https://github.com/dpedrosac/flagshipPPR/
# Inputs:      results/sorted_pdq39.csv
# Outputs:     Objects in memory; optional check for outliers (see code).
## ---------------------------

# ---- Packages ----------------------------------------------------------------
required_pkgs <- c(
  "consort",
  "dplyr",
  "emmeans",
  "ggplot2",
  "janitor",
  "lme4",
  "lmerTest",
  "lubridate",
  "patchwork",
  "performance",
  "scales",
  "purrr",
  "readr",
  "rlang",
  "sjPlot",
  "stringr",
  "survival",
  "tableone",
  "tibble",
  "tidyr",
  "tidyverse"
)

install_missing <- isTRUE(as.logical(Sys.getenv("PPR_INSTALL_PACKAGES", "FALSE")))

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  msg <- paste0(
    "Missing required package(s): ", paste(missing_pkgs, collapse = ", "), "\n",
    "Install them first, e.g.:\n",
    "  install.packages(c(", paste(sprintf('"%s"', missing_pkgs), collapse = ", "), "))\n",
    "Or set environment variable PPR_INSTALL_PACKAGES=TRUE to auto-install."
  )
  if (!install_missing) {
    stop(msg, call. = FALSE)
  }
  install.packages(missing_pkgs, dependencies = TRUE)
}

suppressPackageStartupMessages({
  for (p in required_pkgs) {
    library(p, character.only = TRUE)
  }
})


# ---- # Paths and global settings: --------------------------------------------

# Toggle this to TRUE when you want to see quick checks in the console/Viewer
sanity_check <- FALSE

# Get user name
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

# Define path where results are saved (folder will be created if inexistant)
results_dir <- file.path(base_dir, "results")

if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
  message("📁 Created folder: ", results_dir)
}

# ---- Helpers -----------------------------------------------------------------
assert_file_exists <- function(path) {
  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }
}

message_if <- function(condition, ...) {
  if (isTRUE(condition)) message(...)
}


# ---- # Data preparation: --------------------------------------------
# ---- Load data data into workspace: ------------------------------------------
sorted_pdq39_path <- file.path(results_dir, "sorted_pdq39.csv")

if (!file.exists(sorted_pdq39_path)) {
  message("sorted_pdq39.csv not found; running cleanup script to (re)create it.")
  source(file.path(script_dir, "02_cleanup_data.R"))
}

sorted_pdq39 <- readr::read_csv(sorted_pdq39_path, show_col_types = FALSE)


# ---- Descriptive statistics: -------------------------------------------------

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


# ---- Sanity check for outliers: ----------------------------------------------

sorted_pdq39 <- sorted_pdq39 %>% # a bit less manual text
  dplyr::mutate(
    Timepoint = factor(Timepoint, levels = paste0("T", 0:7))
  )

time_levels <- paste0("T", 0:6) # drop T7

# Make sure data uses these levels (keeps empty timepoints on the axis if needed)
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
