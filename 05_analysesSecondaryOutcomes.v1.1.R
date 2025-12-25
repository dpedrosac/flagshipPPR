#!/usr/bin/env Rscript

## ---------------------------
##
## Script name:  05_analysesSecondaryOutcomes.v1.0.R
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
##   1.0 — 2025-12-08 — First version intending to disentangle analyses.
##
## ---------------------------


################################################################################
# Data preparation
################################################################################

source("02_cleanup_data.v1.3.R") # loads all data into workspace


############################################################################################
# Secondary analyses: UPDRS, MoCA, BDI
############################################################################################

moca_T0_T6 <- moca %>% filter(Timepoint %in% c("T0", "T6"))
bdi_T0_T6 <- bdi %>% filter(Timepoint %in% c("T0", "T6"))
updrs_T0_T6 <- updrs %>% filter(Timepoint %in% c("T0", "T6"))
barthel_T0_T6 <- barthel %>% filter(Timepoint %in% c("T0", "T6"))

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
# Start plots
############################################################################################

make_score_plot <- function(data, score_col, score_label, sanity_check = TRUE) {
  # score_col: character with the column name, e.g. "total_score_moca"
  # score_label: pretty label for axes/annotation, e.g. "MoCA"
  
  score_sym     <- sym(score_col)                     # symbol for the score column
  baseline_name <- paste0(score_col, "_baseline")     # e.g. "total_score_moca_baseline"
  baseline_sym  <- sym(baseline_name)
  
  # 1) Restrict to T0 and T6
  data_T0_T6 <- data %>%
    filter(Timepoint %in% c("T0", "T6"))
  
  # 2) Derive baseline (T0) score per patient
  baseline <- data_T0_T6 %>%
    filter(Timepoint == "T0") %>%
    select(
      patient,
      !!baseline_sym := !!score_sym
    )
  
  # 3) Join baseline back into T0–T6 data
  data_T0_T6 <- data_T0_T6 %>%
    left_join(baseline, by = "patient")
  
  # 4) Summary statistics for mean ± SD
  df_summary <- data_T0_T6 %>%
    group_by(group, Timepoint) %>%
    summarise(
      n    = sum(!is.na(!!score_sym)),
      mean = mean(!!score_sym, na.rm = TRUE),
      sd   = sd(!!score_sym,   na.rm = TRUE),
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
  

  # Dynamic y-limits based on mean ± 2 SD with padding
  y_min_raw <- min(df_summary$mean - 1.25 * df_summary$sd, na.rm = TRUE)
  y_max_raw <- max(df_summary$mean + 1.25 * df_summary$sd, na.rm = TRUE)
  
  y_min <- max(0, y_min_raw)                                   # no negative scores
  y_max <- y_max_raw + 0.10 * (y_max_raw - y_min_raw)          # 10% padding on top
  
  # Height for "n =" labels (just above error bars, but inside plotting area)
  yposition <- y_max_raw -.05 * (y_max_raw - y_min_raw)
  
  # 5) LMM: score ~ baseline + group * Timepoint + (1 | patient)
  model_formula <- as.formula(
    paste0(
      score_col,
      " ~ ",
      baseline_name,
      " + group * Timepoint + (1 | patient)"
    )
  )
  
  model_lmm <- lmer(
    formula = model_formula,
    data    = data_T0_T6
  )
  
  # Extract ANOVA table and pull group:Timepoint stats
  anova_tbl <- anova(model_lmm, type = 3) %>%
    as.data.frame() %>%
    rownames_to_column("term")
  
  gt_row <- anova_tbl %>%
    filter(term == "group:Timepoint")
  
  if (nrow(gt_row) == 1) {
    # Column names differ slightly by version, so be defensive
    f_col <- intersect(c("F.value", "F value"), names(gt_row))
    p_col <- intersect(c("Pr..F.", "Pr(>F)"), names(gt_row))
    
    F_val <- if (length(f_col) == 1) gt_row[[f_col]] else NA_real_
    p_val <- if (length(p_col) == 1) gt_row[[p_col]] else NA_real_
    
    stat_label <- sprintf(
      "Group×Time: F = %.2f, p = %s",
      F_val,
      format.pval(p_val, digits = 2, eps = 0.001)
    )
  } else {
    stat_label <- "Group×Time: n/a"
  }
  
  if (isTRUE(sanity_check)) {
    message("=== LMM ANOVA for ", score_label, " ===")
    print(anova_tbl)
    message("Stats label: ", stat_label)
  }
  
  ##############################################################################
  # Plot: mean ± SD over T0/T6, group-wise, with centered header annotation
  ##############################################################################
  
  dodge_w <- 0.15
  pdodge  <- position_dodge(width = dodge_w)
  
  # x-position for centered "header" text (middle between T0 and T6)
  x_mid <- mean(as.numeric(df_summary$Timepoint))
  
  p <- ggplot(
    df_summary,
    aes(x = Timepoint, y = mean, group = group, linetype = group)
  ) +
    # Error bars: mean ± SD
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
    # Connecting mean lines
    geom_line(
      position  = pdodge,
      linewidth = 1.2,
      color     = "black"
    ) +
    
      # Text labels positioned at x_text
    geom_text(
      data = df_summary,
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
  
    # n-labels at fixed height
    #geom_text(
    #  aes(
    #    x     = Timepoint,
    #    y     = yposition,
    #    label = sprintf("n = %d", n),
    #    group = group
    #  ),
    #  position = position_dodge(width = dodge_w),
    #  vjust    = -0.8,
    #  size     = 4.0,
    #  color    = "black"
    #) +
    # Centered header-like annotation: score label + stats, stacked
    annotate(
      "text",
      x        = x_mid,
      y        = y_max,
      label    = paste0(score_label, "\n", stat_label),
      hjust    = 0.5,
      vjust    = 1.2,
      size     = 6,
      fontface = "bold"
    ) +
    scale_linetype_manual(
      values = c("Control" = "solid", "Intervention" = "dashed")
    ) +
    coord_cartesian(ylim = c(y_min, y_max)) +
    labs(
      title    = NULL,
      x        = "",
      y        = score_label,
      linetype = "Group"
    ) +
    theme_minimal(base_size = 18) +
    theme(
  	legend.position      = c(0.98, 0.05),         # bottom-right corner
  	legend.justification = c(1, 0),               # anchor legend to bottom-right
  	legend.background    = element_rect(
                           fill = alpha("white", 0.8),
                           color = NA
                         ),
  	panel.grid.minor   = element_blank(),
  	panel.grid.major.x = element_blank(),
  	axis.title         = element_text(size = 20),
  	axis.text          = element_text(size = 18),
  	legend.text        = element_text(size = 16),
  	legend.title       = element_text(size = 16)
    ) +
    guides(
      linetype = guide_legend(
      override.aes = list(size = 3)     # increase line thickness in legend
      )
    )
  
  list(
    data_T0_T6 = data_T0_T6,
    summary    = df_summary,
    model      = model_lmm,
    plot       = p
  )
}

################################################################################
# Apply to MoCA, BDI, UPDRS and combine with patchwork
################################################################################

res_moca <- make_score_plot(
  data        = moca,
  score_col   = "total_score_moca",   # column name in `moca`
  score_label = "MoCA"
)

res_bdi <- make_score_plot(
  data        = bdi,
  score_col   = "bdi_score",
  score_label = "BDI-II"
)

res_updrs <- make_score_plot(
  data        = updrs,
  score_col   = "Teil3",
  score_label = "UPDRS-III"
)


res_nmss <- make_score_plot(
  data        = nmss,
  score_col   = "nmss_total",
  score_label = "NMSS"
)


res_updrs1 <- make_score_plot(
  data        = updrs,
  score_col   = "Teil1",   # column name in `moca`
  score_label = "UPDRS-I"
)

res_updrs2 <- make_score_plot(
  data        = updrs,
  score_col   = "Teil2",
  score_label = "UPDRS-II"
)

res_updrs4 <- make_score_plot(
  data        = updrs,
  score_col   = "Teil4",
  score_label = "UPDRS-IV"
)

res_eq5d <- make_score_plot(
  data        = eq5d,
  score_col   = "likert",
  score_label = "EQ5D"
)

res_barthel <- make_score_plot(
  data        = barthel,
  score_col   = "barthel_index",
  score_label = "Barthel"
)


# Combine plots side-by-side with a single shared legend at the bottom
combined_secondary1 <- (
  (res_updrs$plot + theme(legend.position = "none")) +
  (res_bdi$plot   + theme(legend.position = "none")) +
  (res_nmss$plot  + theme(legend.position = "none")) +
  (res_moca$plot  + theme(legend.position = "right"))  # or c(0.98, 0.05) for inside
) +
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Secondary Outcomes: UPDRS-III, BDI-II, NMSS, MoCA",
    theme = theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 22)
    )
  )

# Remove x-axis text/labels for all panels
combined_secondary1 <- combined_secondary1 &
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

# Restore x-axis for bottom row only (panels 3 and 4)
combined_secondary1[[3]] <- combined_secondary1[[3]] +
  theme(
    axis.title.x = element_text(),
    axis.text.x  = element_text(),
    axis.ticks.x = element_line()
  )

combined_secondary1[[4]] <- combined_secondary1[[4]] +
  theme(
    axis.title.x = element_text(),
    axis.text.x  = element_text(),
    axis.ticks.x = element_line()
  )

combined_secondary1

# SVG
ggsave(
  filename = file.path(results_dir, "fig2.secondaryOutcomes.prepost.v1.0.svg"),
  plot     = combined_secondary1,
  device   = "svg",
  width    = 16,
  height   = 18,
  units    = "in"
)

# High-resolution PNG
ggsave(
  filename = file.path(results_dir, "fig2.secondaryOutcomes.prepost.v1.0.png"),
  combined_secondary1,
  device   = "png",
  width    = 16,
  height   = 18,
  dpi      = 600      # high resolution
)


# Combine plots side-by-side with a single shared legend at the bottom
combined_secondary2 <- (
  res_updrs1$plot +
  res_updrs2$plot  +
  res_updrs4$plot
) +
  plot_layout(guides = "collect") +   # one shared legend
  plot_annotation(
    title = "Secondary Outcomes: UPDRS-I, UPDRS-II, UPDRS-IV",
    theme = theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 22)
    )
  ) &
  theme(legend.position = "bottom")   # apply to collected legend

combined_secondary2

