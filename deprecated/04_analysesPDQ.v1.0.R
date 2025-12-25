#!/usr/bin/env Rscript

## ---------------------------
##
## Script name:  03_analyses.v1.0.R
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
##   1.0 — 2025-12-05 — First version intending to start with analyses.
##
## ---------------------------



################################################################################
# Data preparation
################################################################################

source("02_cleanup_data.v1.3.R")

############################################################################################
# Load data data into workspace:
############################################################################################

sorted_pdq39 <- tryCatch(     # First attempt: read the existing CSV
  {
    readr::read_csv(
      file.path(results_dir, "sorted_pdq39.csv"),
      show_col_types = FALSE
    )
  },
  
  error = function(e) {
    message("⚠️ Could not read 'sorted_pdq39.csv': ", e$message)
    message("ℹ Attempting to recreate it via '02_cleanup_data.v1.2.R'...")

    cleanup_ok <- tryCatch(
      {
        source("02_cleanup_data.v1.2.R")
        TRUE
      },
      error = function(e2) {
        message("❌ Error while running '02_cleanup_data.v1.2.R': ", e2$message)
        FALSE
      }
    )

    if (!cleanup_ok) {
      stop("Aborting: failed to (re)create 'sorted_pdq39.csv' via 02_cleanup_data.v1.2.R.")
    }

    # Second attempt: read the CSV again (now it should exist)
    readr::read_csv(
      file.path(results_dir, "sorted_pdq39.csv"),
      show_col_types = FALSE
    )
  }
)

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

if (isTRUE(sanity_check)) { # Only show outputs if sanity_check is TRUE
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
  aes(x = Timepoint, y = pdq39_sum_index, fill = group)
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
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",              # remove redundant legend
    strip.text = element_text(face = "bold"),  # emphasize facet labels
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),    # cleaner grid
    panel.grid.major.x = element_blank()
  )

# 1) Summary statistics for Mean ± SD
# =====================================================================

df_summary <- pdq39_T0_T6 %>%
  group_by(group, Timepoint) %>%
  summarise(
    n    = sum(!is.na(pdq39_sum_index)),
    mean = mean(pdq39_sum_index, na.rm = TRUE),
    sd   = sd(pdq39_sum_index,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Recode T0/T6 to human-readable labels
    Timepoint = recode(
      Timepoint,
      "T0" = "baseline",
      "T6" = "6-months"
    ),

    # Fix ordering on the x-axis
    Timepoint = factor(Timepoint, levels = c("baseline", "6-months")),

    # Numeric x positions
    x_num  = as.numeric(Timepoint),

    # Shift text labels left/right
    x_text = x_num + ifelse(group == "Control", -0.15, 0.15)
  )

# 2) Plot parameters; Mean ± SD with shifted labels
# =====================================================================

dodge_w   <- 0.15
pdodge    <- position_dodge(width = dodge_w)
yposition <- max(df_summary$mean + df_summary$sd)


pdq39_mean_sd <- ggplot(
    df_summary,
    aes(x = Timepoint, y = mean, group = group, linetype = group)
  ) +

  # Error bars: ± SD
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    position  = pdodge,
    width     = 0.05,
    linewidth = 1.1,
    color     = "black"
  ) +

  # Mean points
  geom_point(
    position = pdodge,
    size     = 3,
    color    = "black"
  ) +

  # Mean connecting lines
  geom_line(
    position  = pdodge,
    linewidth = 1.2,
    color     = "black"
  ) +

  # Text labels positioned at x_text
  geom_text(
    data        = df_summary,
    aes(
      x     = x_text,
      y     = yposition,
      label = sprintf("n = %d", n)
    ),
    vjust       = -0.8,
    size        = 6,
    color       = "black",
    inherit.aes = FALSE
  ) +

  # Line type mapping
  scale_linetype_manual(values = c(
    "Control"      = "solid",
    "Intervention" = "dashed"
  )) +

  # Y-axis limits
  coord_cartesian(ylim = c(0, ceiling(yposition + 5))) +

  # Labels
  labs(
    title    = "PDQ-39 Mean ± SD by Timepoint and Group",
    subtitle = "Group trajectories (solid vs. dashed) with ± SD error bars",
    x        = "",
    y        = "PDQ-39 Sum Index",
    linetype = "Group"
  ) +

  # =====================================================================
  # FONT SIZES HERE → CHANGE base_size for overall magnification
  # =====================================================================
  theme_minimal(base_size = 18) +
  theme(
    legend.position    = "right",
    plot.title         = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle      = element_text(size = 18, hjust = 0.5),
    axis.title         = element_text(size = 20),
    axis.text          = element_text(size = 18),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  )

pdq39_mean_sd

# SVG
ggsave(
  file.path(results_dir, "fig1.pdq39.prepost.v1.0.svg"),
  pdq39_mean_sd,
  device = "svg",
  width = 10,
  height = 7
)

# High-resolution PNG
ggsave(
  file.path(results_dir, "fig1.pdq39.prepost.v1.0.png"),
  pdq39_mean_sd,
  device = "png",
  width = 10,
  height = 7,
  dpi = 600      # high resolution
)


############################################################################################
# Main analysis with a Linear mixed model:
############################################################################################

# Adding baseline per patient as covariate to account for differences
pdq_baseline <- pdq39_T0_T6 %>%
  filter(Timepoint == "T0") %>%
  select(
    patient,
    pdq39_baseline = pdq39_sum_index
  )

# Join baseline and drop rowswithout baseline (needed for covariate model)
pdq39_T0_T6_model <- pdq39_T0_T6 %>%
  left_join(pdq_baseline, by = "patient") %>%
  filter(!is.na(pdq39_baseline)) %>%
    mutate(
    # Recode T0/T6 to human-readable labels
    Timepoint = recode(
      Timepoint,
      "T0" = "baseline",
      "T6" = "6-months"
    )
)

# Model WITHOUT baseline covariate
model_lmm <- lmer(
  pdq39_sum_index ~ group * Timepoint + (1 | patient),
  data = pdq39_T0_T6_model,
  REML = FALSE
)

# Model WITH baseline covariate
model_lmm_cov <- lmer(
  pdq39_sum_index ~ pdq39_baseline + group * Timepoint + (1 | patient),
  data = pdq39_T0_T6_model,
  REML = FALSE
)

anova(model_lmm, model_lmm_cov)
performance_lmm_cov <- model_performance(model_lmm_cov)
performance_lmm_cov

model_lmm_cov_reml <- lmer(
  pdq39_sum_index ~ pdq39_baseline + group * Timepoint + (1 | patient),
  data = pdq39_T0_T6_model,
  REML = TRUE
)

summary(model_lmm_cov_reml)
anova(model_lmm_cov_reml, type = 3)

tab_model(
  model_lmm_cov_reml,
  show.re.var = TRUE,
  show.icc = TRUE,
  file = file.path(results_dir, "Tab1.pdq39_model_covariate.v1.0.doc")
)


