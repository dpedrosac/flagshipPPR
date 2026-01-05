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

# ---- # Load necessary data: --------------------------------------------------

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

# ---- Descriptive statistics: -------------------------------------------------

analysis_both <- sorted_pdq39 %>%
  dplyr::group_by(group, Timepoint) %>%           # <- group by both
  dplyr::summarise(
    n = sum(!is.na(pdq39_sum_index)),               #)n        = dplyr::n(),: stamd zuerst da, daduzrch alle weg die NA haben, aber sind ja dummys jetzt stimmt 
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


# ---- Boxplots, group- and time-sorted: -------------------------------------------------


pdq39_T0_T6 <- sorted_pdq39 %>% filter(Timepoint %in% c("T0", "T6")) # only before and after intervention

# Boxplots of two groups (as facets)
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


# ---- Summary statistics for Mean ± SD: -------------------------------------------------

df_summary <- pdq39_T0_T6 %>%
  group_by(group, Timepoint) %>%
  summarise(
    n    = sum(!is.na(pdq39_sum_index)),         #  n    = dplyr::n(),?
    mean = mean(pdq39_sum_index, na.rm = TRUE),
    sd   = sd(pdq39_sum_index,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Timepoint = recode( # Recode T0/T6 to labels that are easier to understand
      Timepoint,
      "T0" = "baseline",
      "T6" = "6-months"
    ),
    Timepoint = factor(Timepoint, levels = c("baseline", "6-months")),
    x_num  = as.numeric(Timepoint),
    x_text = x_num + ifelse(group == "Control", -0.15, 0.15)
  )

# ---- Plot PDQ-39 change over time by group -----------------------------------

# Parameters
dodge_w <- 0.15
pdodge  <- position_dodge(width = dodge_w)

y_position <- max(df_summary$mean + df_summary$sd, na.rm = TRUE)
y_limit    <- ceiling(y_position + 5)

out_base <- file.path(results_dir, "fig1.pdq39.prepost.v1.0")

pdq39_mean_sd <- ggplot(
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
  # Means (points + connecting lines)
  geom_point(position = pdodge, size = 3, color = "black") +
  geom_line(position = pdodge, linewidth = 1.2, color = "black") +
  # Sample size labels
  geom_text(
    aes(x = x_text, y = y_position, label = sprintf("n = %d", n)),
    vjust       = -0.8,
    size        = 6,
    color       = "black",
    inherit.aes = FALSE
  ) +
  # Group linetypes
  scale_linetype_manual(
    values = c(Control = "solid", Intervention = "dashed"),
    name   = "Group"
  ) +
  # Axes and labels
  coord_cartesian(ylim = c(0, y_limit)) +
  labs(
    title    = "PDQ-39 Mean ± SD by Timepoint and Group",
    subtitle = "Group trajectories (solid vs. dashed) with ± SD error bars",
    x        = NULL,
    y        = "PDQ-39 Sum Index"
  ) +
  # Theme
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

# Export SVG and PNG
ggsave(paste0(out_base, ".svg"), plot = pdq39_mean_sd, width = 10, height = 7, device = "svg")
ggsave(paste0(out_base, ".png"), plot = pdq39_mean_sd, width = 10, height = 7, dpi = 600, device = "png")


# ---- Statistical analyses for primary outcome -----------------------------------

# Adding baseline per patient as covariate to account for differences
pdq39_T0_T6_model <- pdq39_T0_T6 %>%
  left_join(
    pdq39_T0_T6 %>%
      filter(Timepoint == "T0") %>%
      transmute(patient, pdq39_baseline = pdq39_sum_index),
    by = "patient"
  ) %>%
  filter(!is.na(pdq39_baseline)) %>%
  mutate(Timepoint = recode(Timepoint, T0 = "baseline", T6 = "6-months"))
  
# Model WITHOUT baseline covariate
model_lmm <- lmer(
  pdq39_sum_index ~ group * Timepoint + (1 | patient),
  data = pdq39_T0_T6_model,
  REML = FALSE
)

# Model WITH baseline covariate, after recentering and scaling
pdq39_T0_T6_centered <- pdq39_T0_T6_model %>%
  mutate(
    tp6 = as.numeric(Timepoint == "6-months"),               # 0/1 indicator
    pdq39_bl_c = as.numeric(scale(pdq39_baseline, center = TRUE, scale = FALSE))
  )

model_lmm_cov <- lmer(
  pdq39_sum_index ~ group * Timepoint + pdq39_bl_c:tp6 + (1 | patient),
  data = pdq39_T0_T6_centered,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

performance_lmm_cov <- model_performance(model_lmm_cov)
performance_lmm_cov

summary(model_lmm_cov)
anova(model_lmm_cov, type = 3)

tab_model(
  model_lmm_cov,
  show.re.var = TRUE,
  show.icc = TRUE,
  file = file.path(results_dir, "Tab1.pdq39_model_covariate.v1.0.doc")
)

# Interpretion of results in written form (adapt in case of changing values/analyses):
cf <- summary(model_lmm_cov)$coefficients

tp  <- "Timepoint6-months"
int <- "groupIntervention:Timepoint6-months"

sprintf(
  paste0(
    "A linear mixed model with random intercepts for patient showed no clear change over time in controls ",
    "(β = %.2f, SE = %.2f, df = %.1f, t = %.2f, p = %.4f, 95%% CI [%.2f, %.2f]), ",
    "and the group × time interaction suggested greater improvement in the intervention group ",
    "(β = %.2f, SE = %.2f, df = %.1f, t = %.2f, p = %.4f, 95%% CI [%.2f, %.2f])."
  ),
  cf[tp, "Estimate"],  cf[tp, "Std. Error"],  cf[tp, "df"],  cf[tp, "t value"],  cf[tp, "Pr(>|t|)"],
  cf[tp, "Estimate"] - 1.96 * cf[tp, "Std. Error"],
  cf[tp, "Estimate"] + 1.96 * cf[tp, "Std. Error"],
  cf[int, "Estimate"], cf[int, "Std. Error"], cf[int, "df"], cf[int, "t value"], cf[int, "Pr(>|t|)"],
  cf[int, "Estimate"] - 1.96 * cf[int, "Std. Error"],
  cf[int, "Estimate"] + 1.96 * cf[int, "Std. Error"]
)

# ---- Optional sanity checks -----------------------------------

if (isTRUE(sanity_check)) {

  # ---- Parameters ------------------------------------------------------------
  MCID <- -4.72

  group_cols <- c(
    "Control"      = "red",
    "Intervention" = "steelblue"
  )

  responder_cols <- c(
    "Non-responder" = "grey60",
    "Responder"     = "darkgreen"
  )

  base_theme <- theme_minimal(base_size = 14)

  # ---- Prepare wide change dataset (robust to T0/T6 vs baseline/6-months) ----
  pdq_change <- pdq39_T0_T6 %>%
    mutate(
      Timepoint = recode(
        Timepoint,
        "baseline" = "T0",
        "6-months" = "T6"
      ),
      group = factor(group, levels = c("Control", "Intervention"))
    ) %>%
    select(patient, group, Timepoint, pdq39_sum_index) %>%
    pivot_wider(
      id_cols     = c(patient, group),
      names_from  = Timepoint,
      values_from = pdq39_sum_index
    ) %>%
    mutate(
      pdq39_T0  = T0,
      pdq39_T6  = T6,
      delta_T6  = pdq39_T6 - pdq39_T0,
      responder = if_else(delta_T6 <= MCID, "Responder", "Non-responder"),
      responder = factor(responder, levels = c("Non-responder", "Responder"))
    )

  # ---- (Optional) Join responder info back into the long dataset -------------
  pdq39_T0_T6 <- pdq39_T0_T6 %>%
    left_join(
      pdq_change %>% select(patient, pdq39_T0, delta_T6, responder),
      by = "patient"
    )

  # ---- Plot 1: T0 vs T6 scatter by group -------------------------------------
  p1 <- ggplot(pdq_change, aes(x = pdq39_T0, y = pdq39_T6, color = group)) +
    geom_point(alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    scale_color_manual(values = group_cols, name = "Group") +
    labs(
      title = "PDQ-39 SI at Baseline (T0) and 6 Months (T6) by Group",
      x     = "PDQ-39 SI (T0)",
      y     = "PDQ-39 SI (T6)"
    ) +
    base_theme

  # ---- Optional: Responder scatter by group (currently disabled) -------------
  # ggplot(pdq_change, aes(x = T0, y = T6, color = factor(responder))) +
  #   geom_point(alpha = 0.7, size = 2) +
  #   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  #   scale_color_manual(
  #     name   = "Responder",
  #     values = c("0" = "grey60", "1" = "darkgreen"),
  #     labels = c("0" = "No MCID achieved", "1" = "MCID achieved")
  #   ) +
  #   facet_wrap(~ group) +
  #   labs(x = "PDQ-39 T0", y = "PDQ-39 T6") +
  #   theme_minimal()

  # ---- Plot 2: Change vs baseline, colored by responder ----------------------
  p2 <- ggplot(pdq_change, aes(x = pdq39_T0, y = delta_T6, color = responder)) +
    geom_point(alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_hline(
      yintercept = MCID,
      linetype   = "dotted",
      color      = responder_cols["Responder"]
    ) +
    scale_color_manual(values = responder_cols, name = "Responder status") +
    labs(
      title = "Change in PDQ-39 SI (T6 − T0) by Baseline and MCID Status",
      x     = "PDQ-39 SI (T0)",
      y     = "Change in PDQ-39 SI (T6 − T0)"
    ) +
    base_theme

  # ---- Plot 3: Histogram of change -------------------------------------------
  p3 <- ggplot(pdq_change, aes(x = delta_T6)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
    geom_vline(
      xintercept = MCID,
      linetype   = "dashed",
      color      = responder_cols["Responder"]
    ) +
    labs(
      title = "Distribution of Change in PDQ-39 SI (T6 − T0)",
      x     = "Change in PDQ-39 SI (T6 − T0)",
      y     = "Number of patients"
    ) +
    base_theme

  # ---- Combine into one 3-panel figure (patchwork) ---------------------------
  fig_pdq_sanity <- (p1 | p2) / p3 +
    plot_annotation(
      title      = "PDQ-39 Sanity Checks and MCID Response",
      subtitle   = sprintf("Responder definition: change ≤ %.2f (MCID)", MCID),
      tag_levels = "A"
    )

  fig_pdq_sanity

  # ---- Optional: Alternative histogram + boxplot panel (disabled) ------------
  # p_hist <- ggplot(pdq_change, aes(x = delta_T6)) +
  #   geom_histogram(
  #     aes(y = ..density..),
  #     bins  = 30,
  #     fill  = "steelblue",
  #     alpha = 0.6,
  #     color = "white"
  #   ) +
  #   stat_function(
  #     fun  = dnorm,
  #     args = list(
  #       mean = mean(pdq_change$delta_T6, na.rm = TRUE),
  #       sd   = sd(pdq_change$delta_T6,   na.rm = TRUE)
  #     ),
  #     color     = "darkorange",
  #     linewidth = 1
  #   ) +
  #   labs(
  #     title = "Distribution of change in PDQ-39 SI (T6 − T0)",
  #     x     = "Change in PDQ-39 SI (T6 − T0)",
  #     y     = "Density"
  #   ) +
  #   theme_minimal()
  #
  # p_box <- ggplot(pdq_change, aes(x = 1, y = delta_T6)) +
  #   geom_boxplot(width = 0.4) +
  #   coord_flip() +
  #   labs(x = NULL, y = NULL) +
  #   theme_minimal() +
  #   theme(
  #     axis.text.y  = element_blank(),
  #     axis.ticks.y = element_blank()
  #   )
  #
  # p_panel <- p_hist / p_box + plot_layout(heights = c(3, 1))
  # p_panel

  # ---- (Optional) Save -------------------------------------------------------
  # ggsave(
  #   file.path(results_dir, "fig_pdq39_sanity_mcid.png"),
  #   fig_pdq_sanity,
  #   width = 12,
  #   height = 9,
  #   dpi = 600
  # )
  # ggsave(
  #   file.path(results_dir, "fig_pdq39_sanity_mcid.svg"),
  #   fig_pdq_sanity,
  #   width = 12,
  #   height = 9,
  #   device = "svg"
  # )
}

#TODO @Antonia: Maybe you can check if this is necessary or not?
# ---- Responder analysis (commented out) --------------------------------------
# Rationale:
# - The outcome "responder" is binary (yes/no), so a binomial GLM is appropriate.
# - Using a logit link allows interpretation in terms of odds ratios (ORs).

# # Show that responder is binary and inspect counts/proportions
# table(pdq39_T0_T6$responder)
# prop.table(table(pdq39_T0_T6$group, pdq39_T0_T6$responder), 1)
#
# # Example interpretation (replace with your computed values if needed):
# # Proportions by group: Control: 1 = 23.4%; Intervention: 1 = 33.7%
# # These proportions are quantified using a GLM (no time variable here).

# # Unadjusted logistic regression (group only)
# glm_responder <- glm(
#   responder ~ group,
#   data   = pdq_change,
#   family = binomial(link = "logit")
# )
# summary(glm_responder)
#
# # Odds ratios with confidence intervals
# exp(cbind(OR = coef(glm_responder), confint(glm_responder)))
# performance::model_performance(glm_responder)

# # Create a summary table of responder counts and within-group percentages
# tab_responder <- pdq_change %>%
#   group_by(group, responder) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   group_by(group) %>%
#   mutate(percent = scales::percent(n / sum(n), accuracy = 0.1)) %>%
#   ungroup() %>%
#   select(group, responder, n, percent) %>%
#   pivot_wider(
#     names_from  = c(group, responder),
#     values_from = c(n, percent),
#     names_sep   = "_"
#   )
#
# print(tab_responder)

# # Forest-style plot of the intervention effect (OR + 95% CI)
# # TODO: OR/CI are hard-coded; maybe extract them from glm_responder?!
# or_data <- data.frame(
#   term  = "Intervention vs. Control",
#   OR    = 1.663,
#   lower = 0.877,
#   upper = 3.195
# )
#
# ggplot(or_data, aes(x = OR, y = term)) +
#   geom_point(size = 4, color = "steelblue") +
#   geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, color = "steelblue") +
#   geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
#   scale_x_continuous(trans = "log10") +
#   labs(
#     x     = "Odds Ratio [95% CI]",
#     title = "MCID Achievement: Intervention Effect"
#   ) +
#   theme_minimal() +
#   theme(axis.title.y = element_blank())

# # Estimated marginal means (predicted probabilities) by group
# emmeans::emmeans(glm_responder, ~ group, type = "response")
# prop.table(table(pdq_change$group, pdq_change$responder), 1)

# # Overdispersion check (Pearson residuals)
# rp   <- residuals(glm_responder, type = "pearson")
# rdf  <- df.residual(glm_responder)
# disp <- sum(rp^2) / rdf
# disp
# # Interpretation: dispersion is consistent with the binomial assumption.

# # Baseline-adjusted logistic regression
# names(pdq_change)
#
# glm_responder_cov <- glm(
#   responder ~ group + pdq39_T0,
#   data   = pdq_change,
#   family = binomial(link = "logit")
# )
# summary(glm_responder_cov)
#
# exp(cbind(OR = coef(glm_responder_cov), confint(glm_responder_cov)))
# performance::model_performance(glm_responder_cov)

# # Predicted probabilities (with CI) by group from the baseline-adjusted model
# emmeans::emmeans(glm_responder_cov, ~ group, type = "response") %>%
#   as.data.frame()

# # Model comparison: unadjusted vs baseline-adjusted
# m0 <- glm(
#   responder ~ group,
#   data   = pdq_change,
#   family = binomial(link = "logit")
# )
# m1 <- glm(
#   responder ~ group + pdq39_T0,
#   data   = pdq_change,
#   family = binomial(link = "logit")
# )
#
# AIC(m0, m1)
# anova(m0, m1, test = "Chisq")
# # If p < 0.001, baseline PDQ is important → prefer m1.

# # Verify how many rows were used by the baseline-adjusted model
# dat_mod <- model.frame(glm_responder_cov)
# nrow(dat_mod)
# length(fitted(glm_responder_cov))
#
# # Example note:
# # AUC = 0.74; acceptable discrimination.

