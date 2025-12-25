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
sanity_check <- TRUE

# Get user name (works on Windows and almost all other systems)
user_name <- Sys.getenv("USERNAME", unset = Sys.info()[["user"]])

# Define base directory depending on user
if (user_name == "akoel") {
  base_dir <- file.path("C:", "Users", "akoel", "OneDrive","Desktop", "Dr. Arbeit")
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
    n    = sum(!is.na(pdq39_sum_index)),         #  n    = dplyr::n(),?
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

install.packages("performance")
library(performance)

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

########################################################################################################
#T0 und T6next to each other to measure difference, if <= -4.72
pdq_change <- pdq39_T0_T6 %>%
  select(patient, group, Timepoint, pdq39_sum_index) %>%
  pivot_wider(
    id_cols    = c(patient, group),
    names_from = Timepoint,          # erzeugt Spalten T0 und T6
    values_from = pdq39_sum_index
  ) %>%
  mutate(
    pdq39_T0  = T0,                  # Baseline-Spalte
    delta_T6  = T6 - T0,             # Change-Score
    responder = ifelse(delta_T6 <= -4.72, 1, 0)   # MCID-Kriterium
  )

# Responder-Info zurück in den Long-Datensatz
pdq39_T0_T6 <- pdq39_T0_T6 %>%
  left_join(
    pdq_change %>% select(patient, pdq39_T0, delta_T6, responder),
    by = "patient"
  )

#############################################################################
#change analysis
#scatterplot nach groups
ggplot(pdq_change, aes(x = T0, y = T6, color = group)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_color_manual(
    name   = "Gruppe",
    values = c("Control" = "chartreuse4", "Intervention" = "steelblue"),
    labels = c("Control" = "Kontroll",
               "Intervention" = "Intervention")
  ) +
  labs(
    title = "PDQ-39-SI zu T0 und T6 nach Gruppe",
    x = "PDQ-39-SI T0",
    y = "PDQ-39-SI T6"
  ) +
  theme_minimal()


#scatterplot  fpr mcid by groups
#dont know yet...could be to much
####ggplot(pdq_change,
####aes(x = T0, y = T6, color = factor(responder))) +
#### geom_point(alpha = 0.7, size = 2) +
####  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#### scale_color_manual(
####    name   = "Responder",
####   values = c("0" = "grey60", "1" = "darkgreen"),
####   labels = c("kein MCID", "MCID erreicht")
####  ) +
####  facet_wrap(~ group) +
####  labs(x = "PDQ-39 T0", y = "PDQ-39 T6") +
####  theme_minimal()


ggplot(pdq_change, aes(x = pdq39_T0, y = delta_T6, color = factor(responder))) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0,     linetype = "dashed") +
  geom_hline(yintercept = -4.72, linetype = "dotted", color = "darkgreen") +
  scale_color_manual(
    name   = "Responder-Status",
    values = c("0" = "grey60", "1" = "darkgreen"),
    labels = c("0" = "kein MCID erreicht",
               "1" = "MCID erreicht")
  ) +
  labs(
    title = "Änderung des PDQ-39-SI (T6−T0) nach Ausgangswert und MCID-Status",
    x     = "PDQ-39-SI T0",
    y     = "Änderung des PDQ-39-SI (T6−T0)"
  ) +
  theme_minimal()




#histogram von delta_T6
ggplot(pdq_change, aes(x = delta_T6)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = -4.72, linetype = "dashed", color = "darkgreen") +
  labs(
    title = "Verteilung der Änderung des PDQ-39-SI (T6−T0)",
    x     = "Änderung des PDQ-39-SI (T6−T0)",
    y     = "Anzahl Patienten"
  ) +
  theme_minimal()

#i guess to much
##p_hist <- ggplot(pdq_change, aes(x = delta_T6)) +
##  geom_histogram(aes(y = ..density..),
##                 bins = 30,
##                 fill = "steelblue",
##                 alpha = 0.6,
##                 color = "white") +
##  stat_function(
##    fun  = dnorm,
##    args = list(
##      mean = mean(pdq_change$delta_T6, na.rm = TRUE),
##     sd   = sd(pdq_change$delta_T6,   na.rm = TRUE)
##    ),
##    color = "darkorange",
##    linewidth = 1
##  ) +
##  labs(
##    title = "Verteilung der Änderung des PDQ-39-SI (T6−T0)",
##    x     = "Änderung des PDQ-39-SI (T6−T0)",
##    y     = "Dichte"
## ) +
##  theme_minimal()
### Horizontaler Boxplot darunter
##p_box <- ggplot(pdq_change, aes(x = 1, y = delta_T6)) +
##  geom_boxplot(width = 0.4) +
##  coord_flip() +
##  labs(x = NULL, y = NULL) +
##  theme_minimal() +
##  theme(
##    axis.text.y  = element_blank(),
##    axis.ticks.y = element_blank()
##  )
### 
##p_panel <- p_hist / p_box + plot_layout(heights = c(3, 1))
##p_panel
###############################################################################################################

#error diststribution
#binary variable: jes/no therefore binomial distribtuion 
# with Link function: logit, cause directly odds ratio interperetation
#to show responder is binary
table(pdq39_T0_T6$responder)
prop.table(table(pdq39_T0_T6$group, pdq39_T0_T6$responder), 1)

#Proportions  per group: Control: 1= 23.4%; Intervention: 1= 33.7%
#roportions will be quantified by  GLM

#glm, weil keine zeitvariable mehr
glm_responder <- glm(
  responder ~ group, 
  data = pdq_change,           
  family = binomial(link = "logit")
)
summary(glm_responder)

exp(cbind(OR = coef(glm_responder), confint(glm_responder)))
performance::model_performance(glm_responder)


tab_responder <- pdq_change %>%
  group_by(group, responder) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(group) %>%                         
  mutate(Prozent = scales::percent(n / sum(n), accuracy = 0.1)) %>%
  ungroup() %>%
  select(group, responder, n, Prozent) %>%
  pivot_wider(names_from = c(group, responder),
              values_from = c(n, Prozent),
              names_sep = "_")


print(tab_responder)

#plot intervention
or_data <- data.frame(
  term = "Intervention vs. Kontrolle",
  OR = 1.663, lower = 0.877, upper = 3.195
)

ggplot(or_data, aes(x = OR, y = term)) +
  geom_point(size = 4, color = "steelblue") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), 
                 height = 0.2, color = "steelblue") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  scale_x_continuous(trans = "log10") +
  labs(x = "Odds Ratio [95%-KI]", 
       title = "MCID-Erreichen: Interventions-Effekt") +
  theme_minimal() +
  theme(axis.title.y = element_blank())

emmeans::emmeans(glm_responder, ~ group, type = "response")
prop.table(table(pdq_change$group, pdq_change$responder), 1)

#overdisperion
rp   <- residuals(glm_responder, type = "pearson")
rdf  <- df.residual(glm_responder)
disp <- sum(rp^2) / rdf
disp
#streuung passt zur binomialen vermutung

#glm baseline adjusted
names(pdq_change)

glm_responder_cov <- glm(
  responder ~ group + pdq39_T0,
  data   = pdq_change,
  family = binomial(link = "logit")
)
summary(glm_responder_cov)

exp(cbind(OR = coef(glm_responder_cov), confint(glm_responder_cov)))
#signifcant intervention effect?1.01

performance::model_performance(glm_responder_cov)

#KI für responder
emmeans::emmeans(glm_responder_cov, ~ group, type = "response") %>%
  as.data.frame()

# comparison
m0 <- glm(responder ~ group,
          data = pdq_change,
          family = binomial(link = "logit"))
m1 <- glm(responder ~ group + pdq39_T0,
          data = pdq_change,
          family = binomial(link = "logit"))

AIC(m0, m1)
anova(m0, m1, test = "Chisq")
#Likelihood (p < 0.001), BaselindePDQ important
#take m1

#possibility to seperate between responders and non-responders. 
install.packages("pROC")
library(pROC)

dat_mod <- model.frame(glm_responder_cov)

nrow(dat_mod)          
length(fitted(glm_responder_cov))

#AUC= 0.74; acceptable
