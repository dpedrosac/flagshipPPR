### Preamble
# Define the packages you want in a list, so that you are not writing them twice
pkgs <- c(
  "dplyr", "emmeans", "lubridate", "ggplot2", "janitor",
  "lme4", "readr", "stringr", "survival", "tidyr", "tidyverse"
)

# Install any packages that are not yet installed coming from that list
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
}

# Load packages ina loop (to avoids long chains of "library(...)-calls")
for (p in pkgs) {
  library(p, character.only = TRUE)
}

# The next lines are so that we both can work with the same scripts and only 
# the base_dir is modified according to our respective systems; just one little 
# modification I would ask you for. Please let's put all raw data into a folder
# called "raw_data". That makes the strucure easiert to understand. We will 
# create further folders for results, etc.

# Get the user name (works on Windows and most systems)
user_name <- Sys.getenv("USERNAME", unset = Sys.info()[["user"]])

# You can extend this section with more else-if blocks for other users
if (user_name == "akoel") {
  # Example: your own account
  base_dir <- file.path("C:", "Users", "akoel", "OneDrive")
  
} else if (user_name == "david") {
  # Example: a student's account on the same computer
  base_dir <- file.path("~", "Schreibtisch",  "PPR_pdq39")
  
} else {
  # Fallback: use the user's home directory
  base_dir <- path.expand("~")
  
  message("⚠️ Unknown user: ", user_name, 
          ". Using default home directory instead.")
}

############################################################################################
# Load data data into workspace:
############################################################################################

pdq39_T0_T6 <- readr::read_csv( # PDQ39 baseline and follow-up data
  file.path(base_dir, "raw_data", "pdq39_T0_T6.csv"),
  show_col_types = FALSE
)

control_df <- readr::read_csv( # Control group data
  file.path(base_dir, "raw_data", "control_df.csv"),
  show_col_types = FALSE
)

intervention_df <- readr::read_csv( # Intervention group data
  file.path(base_dir, "raw_data", "intervention_df.csv"),
  show_col_types = FALSE
)

sorted_pdq39 <- readr::read_csv( # Already sorted PDQ39 data
  file.path(base_dir, "raw_data", "sorted_pdq39.csv"),
  show_col_types = FALSE
)

#TODO: You can certainly do this that way, but from a data perspective it's unnecessary complicated. 
# If you just keep sorted_pdq39, you should be able to get:

# a) control_df with 

# control_df <- sorted_pdq39 %>% filter(Timepoint %in% c("T0", "T6")) %>% filter(Group == "Control")

# b) intervention_df with 

# control_df <- sorted_pdq39 %>% filter(Timepoint %in% c("T0", "T6")) %>% filter(Group == "Intervention")

# c) pdq39_T0_T6 with 

# pdq39_T0_T6 <- sorted_pdq39 %>% filter(Timepoint %in% c("T0", "T6"))

# Later you may want to plot e.g. the boxplots for both groups separately, you can use the entire data 
# (see lines 125f. for the example)

filtered_pdq <- sorted_pdq39 %>%
  filter(Timepoint %in% c("T0", "T6"))

############################################################################################
# Descriptive statistics:
############################################################################################

# Toggle this to TRUE when you want to see quick checks in the console/Viewer
sanity_check <- FALSE   # (was misspelled as 'sanity_chekc')

analysis_intervention <- intervention_df %>%
  dplyr::group_by(Timepoint) %>%
  dplyr::summarise(
    # count non-missing values for this variable
    n        = sum(!is.na(pdq39_sum_index)),
    Mean     = mean(pdq39_sum_index, na.rm = TRUE),
    Median   = median(pdq39_sum_index, na.rm = TRUE),
    SD       = stats::sd(pdq39_sum_index, na.rm = TRUE),
    SE       = SD / sqrt(n),                         # standard error from actual n
    Minimum  = min(pdq39_sum_index, na.rm = TRUE),
    Q1       = stats::quantile(pdq39_sum_index, 0.25, na.rm = TRUE, names = FALSE),
    IQR      = stats::IQR(pdq39_sum_index, na.rm = TRUE),
    Q3       = stats::quantile(pdq39_sum_index, 0.75, na.rm = TRUE, names = FALSE),
    Maximum  = max(pdq39_sum_index, na.rm = TRUE),
    Variance = stats::var(pdq39_sum_index, na.rm = TRUE),
    # Tukey fences (useful for outlier checks)
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR,
    .groups = "drop"
  )

# Only show outputs if sanity_check is TRUE
if (isTRUE(sanity_check)) {
  print(analysis_intervention)                # console preview
  if (interactive()) utils::View(analysis_intervention, title = "Intervention descriptives")
}

#boxplot intervention T0 und T6
boxplot_intervention <- ggplot(intervention_df, aes(x = Timepoint, y = pdq39_sum_index)) + 
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue")+
  labs(title = "Boxplot PDQ39 Scores (Intervention)", x = "Zeitpunkt", y = "PDQ39 Sum Index")+
  theme_minimal()

# TODO: example for boxplots of two groups (as facets)
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

# TODO: or here is an example of the boxplots per Timepoint:
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

# TODO: The point is, having too many variables is more prone to copy errors/old versions/etc. and it makes it more difficult 
# to visualise group data etc. (see below)
  
#TODO: What you do with the filename is sort of a hard coded version, which works but may be problematic. 
# Let's use the dynamic folders we defined before replacing this:
# ggsave(filename = "C:/Users/akoel/OneDrive/Desktop/Dr. Arbeit/PDQ39/deskriptive statistik/original daten/boxplot_intervention.png",
#      plot = boxplot_intervention, width = 8, height = 6, dpi = 300)
#
# with 
#

# Define where to save results (this folder will be created if it doesn't exist and this part could be on top)
results_dir <- file.path(base_dir, "results")

if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
  message("📁 Created folder: ", results_dir)
}

# Filename within that folder to save
plot_path <- file.path(results_dir, "boxplot_intervention.png")

# Save the plot (e.g., the improved boxplot_intervention or boxplot_pdq39_groupside)
ggsave(
  filename = plot_path,
  plot     = boxplot_intervention,   # or whatever your plot object is called
  width    = 8,
  height   = 6,
  dpi      = 300
)


#histogram (für verteilung) <- TODO: I think the histograms are not necessary with what I would like to propose later (see line 360ff.)
histogram_intervention <- ggplot(intervention_df, aes(x = pdq39_sum_index)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white", alpha = 0.8) +
  facet_wrap(~ Timepoint) +
  labs(title = "Histogramm PDQ39 Sum Index (Intervention)",
       x = "PDQ39 Sum Index", y = "Anzahl Patienten") +
  theme_minimal()
ggsave(-filename = "C:/Users/akoel/OneDrive/Desktop/Dr. Arbeit/PDQ39/deskriptive statistik/original daten/histogram_intervention.png",
       plot = histogram_intervention, width = 8, height = 6, dpi = 300)

#Anazahl T0 intervention # TODO: not necessary, you have that in analysis both as "n"
n_T0 <- intervention_df %>%
  filter(Timepoint == "T0") %>%
  summarise(n = n())
print(n_T0)
#Anzahl kontrol t0
nc_T0 <- control_df %>%
  filter(Timepoint == "T0") %>%
  summarise(n =n())
print(nc_T0)
library(dplyr)

#descrptive control

#TODO: Same here (sorry, should have read the code before commenting before. Here is a suggestion on the descriptive data without splitting data:

analysis_both <- pdq39_T0_T6 %>%
  dplyr::group_by(group, Timepoint) %>%           # <- group by BOTH
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


analysis_control <- control_df %>%
  group_by(Timepoint) %>%
  reframe( Mean = mean(pdq39_sum_index, na.rm = TRUE),
           Median = median(pdq39_sum_index, na.rm = TRUE),
           SD = sd(pdq39_sum_index, na.rm = TRUE),
           Minimum = min(pdq39_sum_index, na.rm = TRUE),
           Maximum = max(pdq39_sum_index, na.rm = TRUE),
           Variance = var(pdq39_sum_index, na.rm = TRUE),
           SE = SD / sqrt(102),
           n = n(),
           IQR = IQR(pdq39_sum_index, na.rm = TRUE),Q1 = quantile(pdq39_sum_index, 0.25, na.rm = TRUE),
           Q3 = quantile(pdq39_sum_index, 0.75, na.rm = TRUE),
           lower = Q1 - 1.5 * IQR,
           upper = Q3 + 1.5 * IQR
  )
print(analysis_control)
view(analysis_control)

#boxplot control T0 und T6
boxplot_control <- ggplot(control_df, aes(x = Timepoint, y = pdq39_sum_index)) + 
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "chartreuse4")+
  labs(title = "Boxplot PDQ39 Scores (Control)", x = "Zeitpunkt", y = "PDQ39 Sum Index")+
  theme_minimal()

ggsave(filename = "C:/Users/akoel/OneDrive/Desktop/Dr. Arbeit/PDQ39/deskriptive statistik/original daten/boxplot_control.png",
       plot = boxplot_control, width = 8, height = 6, dpi = 300)

#histogram (für verteilung), control
histogram_control <- ggplot(control_df, aes(x = pdq39_sum_index)) +
  geom_histogram(binwidth = 5, fill = "chartreuse4", color = "white", alpha = 0.8) +
  facet_wrap(~ Timepoint) +
  labs(title = "Histogramm PDQ39 Sum Index (Control)",
       x = "PDQ39 Sum Index", y = "Anzahl Patienten") +
  theme_minimal()
ggsave(filename = "C:/Users/akoel/OneDrive/Desktop/Dr. Arbeit/PDQ39/deskriptive statistik/original daten/histogram_control.png",
       plot = histogram_control, width = 8, height = 6, dpi = 300)

#boxplot für beidein einem
boxplot_combined <- ggplot(combined_df, aes(x = Timepoint, y = pdq39_sum_index, fill = group)) +
  geom_boxplot(position = position_dodge(0.8), outlier.colour = "red", outlier.size = 2) +
  scale_fill_manual(values = c("Intervention" = "steelblue", "Control" = "chartreuse4")) +
  labs(title = "Vergleich PDQ39 Scores Intervention vs Kontrolle",
       x = "Zeitpunkt", y = "PDQ39 Sum Index", fill = "Gruppe") +
  theme_minimal()
ggsave(filename = "C:/Users/akoel/OneDrive/Desktop/Dr. Arbeit/PDQ39/deskriptive statistik/original daten/boxplot_combined.png",
       plot = boxplot_combined, width = 8, height = 6, dpi = 300)

#fehlerbalken mit +/- 2Sd (95%)
df_summary <- combined_df %>%
  group_by(group, Timepoint) %>%
  summarise(
    mean = mean(pdq39_sum_index, na.rm = TRUE),
    sd = sd(pdq39_sum_index, na.rm = TRUE),
    n = n(),
    se = sd(pdq39_sum_index, na.rm = TRUE) / sqrt((n)),
    .groups = "drop"
  )
mean_sd <- ggplot(df_summary, aes(x = Timepoint, y = mean, color = group, group = group)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) + 
  geom_errorbar(aes(ymin = mean - 2*sd, ymax = mean + 2*sd), width = 0.2, position = position_dodge(width = 0.3)) + 
  labs(title = "PDQ39 Mittelwert mit Fehlerbalken ± 2SD",
       x = "Zeitpunkt",
       y = "Mean PDQ39 (± 2SD)",
       color = "Gruppe") +
  theme_minimal()
ggsave(filename = "C:/Users/akoel/OneDrive/Desktop/Dr. Arbeit/PDQ39/deskriptive statistik/original daten/mean_sd.png",
       plot = mean_sd, width = 8, height = 6, dpi = 300)

mean_se <- ggplot(df_summary, aes(x = Timepoint, y = mean, color = group, group = group)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(width = 0.3)) +
  labs(title = "PDQ39 Mittelwert ± SE Intervention vs Kontrolle",
       x = "Zeitpunkt",
       y = "Mittelwert PDQ39 (± SE)",
       color = "Gruppe") +
  theme_minimal()
ggsave(filename = "C:/Users/akoel/OneDrive/Desktop/Dr. Arbeit/PDQ39/deskriptive statistik/original daten/mean_se.png",
       plot = mean_se, width = 8, height = 6, dpi = 300)

#graphik t0-t6
file1 <- "C:/Users/akoel/OneDrive/Desktop/Dr. Arbeit/PDQ39/Fragebogen pdq39/sorted_pdq39.csv"
sorted_pdq39 <- read.csv(file1) # TODO: That was on top already. You can remove it here

# sorted_pdq39$Timepoint <- factor(sorted_pdq39$Timepoint, levels = c("T0","T1","T2","T3","T4","T5","T6","T7"))
sorted_pdq39 <- sorted_pdq39 %>% # a bit less manual text
  dplyr::mutate(
    Timepoint = factor(Timepoint, levels = paste0("T", 0:7))
  )


## Proposed version of a plot with the option to look for outliers:

# 0) Time levels: drop T7
time_levels <- paste0("T", 0:6)

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




## NOT NECESSARY

means_pdq <-sorted_pdq39 %>%
  group_by(group, Timepoint) %>%
  summarise(mean_pdq = mean(pdq39_sum_index, na.rm = TRUE)) %>%
  ungroup()
ggplot(means_pdq, aes(x = Timepoint, y = mean_pdq, color = group, group = group)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "PDQ39 Mittelwert nach Gruppe und Zeitpunkt",
       x = "Zeitpunkt",
       y = "Mittelwert PDQ39 Sum Index",
       color = "Gruppe") +
  theme_minimal()
# unique(sorted_pdq39$Timepoint)
# table(sorted_pdq39$Timepoint)
#ggsave(
#  filename = "C:/Users/akoel/OneDrive/Desktop/Dr. Arbeit/PDQ39/deskriptive statistik/original daten/pdq39_mittelwerte.png",
#  plot = last_plot(),
#  width = 8,
#  height = 6,
#  units = "in",
#  dpi = 300
#)

