#!/usr/bin/env Rscript

# ---------------------------
#
# Script name: 03_createTableOne.R
#
# Purpose of script: Create a TableOne for the project after data cleanup is finished 
# (for details see [02_cleanup_data.R]).
#
# Authors:	Antonia Koelble, Anna Pedrosa, Hanna Fischer, David Pedrosa
#
# ---------------------------
#
# Notes:
#   - Project: ParkProReakt (2022–2025)
#   - GitHub repository: https://github.com/dpedrosac/flagshipPPR/
#
# ---------------------------

# ---- # Tidy up code: --------------------------------------------------------

# runs necessary analyses if data is not in the workspace
if (!exists("demographics_pat") || !is.data.frame(demographics_pat)) {
  message("Object missing or invalid — running cleanup script.")
  source("02_cleanup_data.V1.3.R")
}

# As "joins" may create group.x / group.y; standardise to a single `group`.
if (!("group" %in% names(demographics_pat))) {
  if (all(c("group.y", "group.x") %in% names(demographics_pat))) {
    demographics_pat <- demographics_pat %>%
      mutate(group = dplyr::coalesce(group.y, group.x)) %>%
      select(-group.x, -group.y)
  } else if ("group.y" %in% names(demographics_pat)) {
    demographics_pat <- demographics_pat %>%
      rename(group = group.y)
  } else if ("group.x" %in% names(demographics_pat)) {
    demographics_pat <- demographics_pat %>%
      rename(group = group.x)
  }
}

demographics_pat <- demographics_pat %>%
  mutate(group = factor(group, levels = c("Control", "Intervention")))

## Tidyup code
demographics_pat <- demographics_pat %>%
  mutate(
    sex_female = case_when(
      sex_at_birth %in% c("Female", "F", "f") ~ 1,
      sex_at_birth %in% c("Male", "M", "m")   ~ 0,
      TRUE ~ NA_real_
    ),
    # Turn into factor: first level = "Female"
    sex_female = factor(
      sex_female,
      levels = c(1, 0),
      labels = c("Female", "Male")
    )
  )

demographics_pat <- demographics_pat %>%
  mutate(
    native_language_recoded = case_when(
      native_language %in% c("Deutsch", "German", "DE", "de") ~ "German",
      is.na(native_language) ~ NA_character_,
      TRUE ~ "Other"
    ),
    native_language_recoded = factor(
      native_language_recoded,
      levels = c("German", "Other")
    )
  )

demographics_pat <- demographics_pat %>%
  rename(
    UPDRS_I   = Teil1,
    UPDRS_II  = Teil2,
    UPDRS_III = Teil3,
    UPDRS_IV  = Teil4
  )
  	
tab_city <- CreateTableOne(
  vars = c(
    "age_years", "sex_female","marital_status","housing",
    "native_language_recoded","education", "hoehnyahrlevel",
    "employment_status", "UPDRS_I", "UPDRS_II", "UPDRS_III", "UPDRS_IV",
    "total_score_moca", "bdi_score", "nmss_total"
  ),
  strata = "group",
  data = demographics_pat,
  factorVars = c(
    "sex_female", "marital_status", "housing", "native_language_recoded","education",
    "employment_status", "hoehnyahrlevel"
  )
)

print(
  tab_city,
  showAllLevels = FALSE,
  missing = TRUE,
  formatOptions = list(big.mark=".", decimal.mark=",")
)

mat <- print(
    tab_city,
    showAllLevels = TRUE,
    smd = FALSE,
    missing = TRUE,
    printToggle = FALSE,
    formatOptions = list(big.mark = ".", decimal.mark = ",")
   )
   
df_tab_city <- as.data.frame(mat, stringsAsFactors = FALSE)
df_tab_city <- tibble::rownames_to_column(df_tab_city, "Variable")

df_clean <- df_tab_city %>%
  # Replace repeated variable names with blanks (for compact tables)
  mutate(
    Variable = ifelse(Variable == lag(Variable), "", Variable),

    # Indent rows that represent factor levels
    Variable = ifelse(
      str_detect(Variable, "^ "),   # already indented
      Variable,
      ifelse(
        Variable == "", "",         # keep empty rows clean
        ifelse(
          str_detect(lag(Variable, default = ""), ":"),
          paste0("   ", Variable),
          Variable
        )
      )
    )
  )

write.csv(
  df_clean,
  file.path(results_dir, "Tab1.demographics_tableone.v1.0.csv"),
  row.names = FALSE,
  na = ""
)

# ---- # Construct dataset for flowchart logic: -------------------------------

## 1) Top box
g <- add_box(
  txt = "Randomised (n = 211)"
)

## 2) Split into Intervention / Control
g <- add_split(
  g,
  txt = c(
    "Allocated to Intervention (n = 108)",
    "Allocated to Control (n = 103)"
  )
)

## 3) Lost to follow-up (side boxes)
g <- add_side_box(
  g,
  txt = c(
    paste0(
      "Lost to follow-up (n = 8):\n",
      "• Study too demanding (2)\n",
      "• Felt insufficiently supported (1)\n",
      "• Technical problems (2)\n",
      "• Family-related problems (1)\n",
      "• Deceased (1)\n",
      "• Health-related problems (1)"
    ),
    paste0(
      "Lost to follow-up (n = 4):\n",
      "• Refused further contact (2)\n",
      "• Study too demanding (1)\n",
      "• No reason provided (1)"
    )
  )
)

## 4) Completed per arm
g <- add_box(
  g,
  txt = c(
    "Completed intervention (n = 100)",
    "Completed control (n = 99)"
  )
)

## 5) Final total (optional)
#g <- add_box(
#  g,
#  txt = "Completed study (n = 199)"
#)

## 6) Plot on screen
g

## 7) Save as SVG and PNG
p <- build_grid(g)

ggsave(
  filename = file.path(results_dir, "fig2.flowchartPPR.v1.0.svg"),
  plot     = p,
  device   = "svg",
  width    = 8,
  height   = 13
)

ggsave(
  filename = file.path(results_dir, "fig2.flowchartPPR.v1.0.png"),
  plot     = p,
  device   = "png",
  dpi      = 600,
  width    = 8,
  height   = 13
)

