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

#!/usr/bin/env Rscript
# =============================================================================
# Script Name:  04_analysespdq.V1.1.R
# Purpose:      Primary endpoint analyses for PDQ-39 (descriptives, plots, and linear mixed models).
#
# Author(s):    Antonia Koelble; David Pedrosa
#
# Notes:
# Project:     ParkProReakt (2022–2025)
# Repository:  https://github.com/dpedrosac/flagshipPPR/
# Inputs:     results/sorted_pdq39.csv (created by 02_cleanup_data.R)
# Outputs:    figures and model tables written to results/ (see ggsave/tab_model).
# =============================================================================


# ---- # Load necessary data for secondary analyses and restrict to T0/T6: -----

# runs necessary analyses if data is not in the workspace
if (!exists("sorted_pdq39") || !is.data.frame(sorted_pdq39)) {
  message("Object missing or invalid — running cleanup script.")
  source("02_cleanup_data.V1.4.R")
}

# Restrict to T0/T6 (optional; make_score_plot() also filters)
moca_T0_T6    <- moca    %>% dplyr::filter(Timepoint %in% c("T0", "T6"))
bdi_T0_T6     <- bdi     %>% dplyr::filter(Timepoint %in% c("T0", "T6"))
updrs_T0_T6   <- updrs   %>% dplyr::filter(Timepoint %in% c("T0", "T6"))
barthel_T0_T6 <- barthel %>% dplyr::filter(Timepoint %in% c("T0", "T6"))

# ---- Plot helper: mean ± SD + LMM interaction label --------------------------

make_score_plot <- function(data, score_col, score_label, sanity_check = TRUE) {
  # data:       long data with columns patient, group, Timepoint, and score_col
  # score_col:  character, e.g. "total_score_moca"
  # score_label: axis label / panel label, e.g. "MoCA"

  score_sym     <- rlang::sym(score_col)
  baseline_name <- paste0(score_col, "_baseline")
  baseline_sym  <- rlang::sym(baseline_name)

  # 1) Restrict to T0 and T6
  data_T0_T6 <- data %>%
    dplyr::filter(Timepoint %in% c("T0", "T6"))

  # 2) Baseline score per patient (T0)
  baseline <- data_T0_T6 %>%
    dplyr::filter(Timepoint == "T0") %>%
    dplyr::select(patient, !!baseline_sym := !!score_sym)

  # 3) Join baseline back
  data_T0_T6 <- data_T0_T6 %>%
    dplyr::left_join(baseline, by = "patient")

  # 4) Summary statistics (mean ± SD)
  df_summary <- data_T0_T6 %>%
    dplyr::group_by(group, Timepoint) %>%
    dplyr::summarise(
      n    = sum(!is.na(!!score_sym)),
      mean = mean(!!score_sym, na.rm = TRUE),
      sd   = sd(!!score_sym,   na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Timepoint = dplyr::recode(Timepoint, "T0" = "baseline", "T6" = "6-months"),
      Timepoint = factor(Timepoint, levels = c("baseline", "6-months")),
      x_num     = as.numeric(Timepoint),
      x_text    = x_num + ifelse(group == "Control", -0.15, 0.15)
    )

  # Dynamic y-limits based on mean ± 1.25 SD (with padding)
  y_min_raw <- min(df_summary$mean - 1.25 * df_summary$sd, na.rm = TRUE)
  y_max_raw <- max(df_summary$mean + 1.25 * df_summary$sd, na.rm = TRUE)

  y_min <- max(0, y_min_raw)
  y_max <- y_max_raw + 0.10 * (y_max_raw - y_min_raw)
  y_pos <- y_max_raw - 0.05 * (y_max_raw - y_min_raw)

  # 5) LMM: score ~ baseline + group * Timepoint + (1 | patient)
  model_formula <- stats::as.formula(paste0(
    score_col, " ~ ", baseline_name, " + group * Timepoint + (1 | patient)"
  ))

  model_lmm <- lme4::lmer(formula = model_formula, data = data_T0_T6)

  # Extract group×time interaction stats from type-III ANOVA
  anova_tbl <- anova(model_lmm) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term")

  gt_row <- dplyr::filter(anova_tbl, term == "group:Timepoint")

  stat_label <- "Group×Time: n/a"
  if (nrow(gt_row) == 1) {
    f_col <- intersect(c("F.value", "F value"), names(gt_row))
    p_col <- intersect(c("Pr..F.", "Pr(>F)"), names(gt_row))

    F_val <- if (length(f_col) == 1) gt_row[[f_col]] else NA_real_
    p_val <- if (length(p_col) == 1) gt_row[[p_col]] else NA_real_

    stat_label <- sprintf(
      "Group×Time: F = %.2f, p = %s",
      F_val,
      format.pval(p_val, digits = 2, eps = 0.001)
    )
  }

  if (isTRUE(sanity_check)) {
    message("=== LMM ANOVA for ", score_label, " ===")
    print(anova_tbl)
    message("Stats label: ", stat_label)
  }

  # Plot: mean ± SD / n-labels / header annotations
  dodge_w <- 0.15
  pdodge  <- ggplot2::position_dodge(width = dodge_w)
  x_mid   <- mean(as.numeric(df_summary$Timepoint))

  p <- ggplot2::ggplot(
    df_summary,
    ggplot2::aes(x = Timepoint, y = mean, group = group, linetype = group)
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean - sd, ymax = mean + sd),
      position  = pdodge,
      width     = 0.05,
      linewidth = 1.1,
      color     = "black"
    ) +
    ggplot2::geom_point(position = pdodge, size = 3, color = "black") +
    ggplot2::geom_line(position = pdodge, linewidth = 1.2, color = "black") +
    ggplot2::geom_text(
      ggplot2::aes(x = x_text, y = y_pos, label = sprintf("n = %d", n)),
      vjust       = -0.8,
      size        = 6,
      color       = "black",
      inherit.aes = FALSE
    ) +
    ggplot2::annotate(
      "text",
      x        = x_mid,
      y        = y_max,
      label    = paste0(score_label, "\n", stat_label),
      hjust    = 0.5,
      vjust    = 1.2,
      size     = 6,
      fontface = "bold"
    ) +
    ggplot2::scale_linetype_manual(values = c(Control = "solid", Intervention = "dashed")) +
    ggplot2::coord_cartesian(ylim = c(y_min, y_max)) +
    ggplot2::labs(title = NULL, x = NULL, y = score_label, linetype = "Group") +
    ggplot2::theme_minimal(base_size = 18) +
    ggplot2::theme(
      legend.position      = c(0.98, 0.05),
      legend.justification = c(1, 0),
      legend.background    = ggplot2::element_rect(
        fill  = scales::alpha("white", 0.8),
        color = NA
      ),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title         = ggplot2::element_text(size = 20),
      axis.text          = ggplot2::element_text(size = 18),
      legend.text        = ggplot2::element_text(size = 16),
      legend.title       = ggplot2::element_text(size = 16)
    ) +
    ggplot2::guides(
      linetype = ggplot2::guide_legend(override.aes = list(linewidth = 1.2))
    )

  list(
    data_T0_T6 = data_T0_T6,
    summary    = df_summary,
    model      = model_lmm,
    plot       = p
  )
}

# ---- Run secondary outcomes ---------------------------------------------------

res_moca    <- make_score_plot(moca,    "total_score_moca", "MoCA")
res_bdi     <- make_score_plot(bdi,     "bdi_score",        "BDI-II")
res_nmss    <- make_score_plot(nmss,    "nmss_total",       "NMSS")
res_eq5d    <- make_score_plot(eq5d,    "likert",           "EQ-5D")
res_barthel <- make_score_plot(barthel, "barthel_index",    "Barthel")

res_updrs   <- make_score_plot(updrs, "Teil3", "UPDRS-III")
res_updrs1  <- make_score_plot(updrs, "Teil1", "UPDRS-I")
res_updrs2  <- make_score_plot(updrs, "Teil2", "UPDRS-II")
res_updrs4  <- make_score_plot(updrs, "Teil4", "UPDRS-IV")

# ---- Combine plots [patchwork] -----------------------------------------------

# Figure 2a: UPDRS-III, BDI-II, NMSS, MoCA
combined_secondary1 <- (
  (res_updrs$plot + ggplot2::theme(legend.position = "none")) +
  (res_bdi$plot   + ggplot2::theme(legend.position = "none")) +
  (res_nmss$plot  + ggplot2::theme(legend.position = "none")) +
  (res_moca$plot  + ggplot2::theme(legend.position = "right"))
) +
  patchwork::plot_layout(ncol = 2) +
  patchwork::plot_annotation(
    title = "Secondary Outcomes: UPDRS-III, BDI-II, NMSS, MoCA",
    theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 22))
  )

# Remove x-axis labels for all panels
combined_secondary1 <- combined_secondary1 &
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.text.x  = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank()
  )

# Restore x-axis for bottom row only (panels 3 and 4)
combined_secondary1[[3]] <- combined_secondary1[[3]] +
  ggplot2::theme(axis.title.x = ggplot2::element_text(),
                 axis.text.x  = ggplot2::element_text(),
                 axis.ticks.x = ggplot2::element_line())

combined_secondary1[[4]] <- combined_secondary1[[4]] +
  ggplot2::theme(axis.title.x = ggplot2::element_text(),
                 axis.text.x  = ggplot2::element_text(),
                 axis.ticks.x = ggplot2::element_line())

combined_secondary1

ggplot2::ggsave(
  filename = file.path(results_dir, "fig2.secondaryOutcomes.prepost.v1.0.svg"),
  plot     = combined_secondary1,
  device   = "svg",
  width    = 16,
  height   = 18,
  units    = "in"
)

ggplot2::ggsave(
  filename = file.path(results_dir, "fig2.secondaryOutcomes.prepost.v1.0.png"),
  plot     = combined_secondary1,
  device   = "png",
  width    = 16,
  height   = 18,
  dpi      = 600
)

# Figure 2b: UPDRS-I, UPDRS-II, UPDRS-IV, Barthel Index
combined_secondary2 <- (
  res_updrs1$plot +
  res_updrs2$plot +
  res_updrs4$plot +
  res_barthel$plot
) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(
    title = "Secondary Outcomes: UPDRS-I, UPDRS-II, UPDRS-IV, Barthel Index",
    theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 22))
  ) &
  ggplot2::theme(legend.position = "bottom")

combined_secondary2
