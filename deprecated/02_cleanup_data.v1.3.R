#!/usr/bin/env Rscript

## ---------------------------
##
## Script name: 02_cleanup_data.R
##
## Purpose of script: Clean and recode ParkProReakt raw data (PDQ-39, BDI, MoCA, UPDRS,
##                    Hoehn & Yahr, demographics) into analysis-ready datasets.
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


# This is code to analyse the ParkProReakt results (project from 2022 -2025)
# Code developed by Antonia Koelble and  David Pedrosa
#
# Version 1.3  # 2025-12-06 # version including handling of dummy variables, fixed error at nmss coding,
#	       # and errors at the way dates were handled.		
# Version 1.2  # 2025-11-26 # Changes in structure and added further questionnaires


################################################################################
# Excluded patients: forgot  a patient and added them
################################################################################

exclude_patients <- c(
  "ZL4A0VBU", "0TOLUW4TF", "AKUK06XMH", "MTPNPZCIY", "G1QP3RGZ5", "0TRNBST3B" , 
  "UP6W00S8", "A33LV19Y", "BNV3HYYX", "8VUZMN38",
  "2JS3X4L6", "8ZCQJQQW", "EX8AI06G", "I7UFJIW1",
  "IAA50B1K", "X8QUGGZ3", "J1TGCNP9", "U3PVC5MF"
)

################################################################################
# ID data and age
################################################################################

randomisation_list = read.csv(file.path(base_dir, "raw_data", "randomisation_table.csv"))

randomisation_list <- randomisation_list %>%
  mutate(
    across(
      c(birthdate, inclusion_date),
      ~ dmy(.x, quiet = TRUE)
    ),
    age_years = time_length(interval(birthdate, inclusion_date), "years")
  )



################################################################################
# Group data (patient-level group assignment)
################################################################################

## Group data
data_group_pat <- read.csv(
  file.path(base_dir, "raw_data", "export_patients_5_2025-10-13-09-52_demapped.csv")
)

group_data <- data_group_pat %>%
  rename(patient = patient_id) %>%
  mutate(
    active_clean = active %>%
      as.character() %>%
      trimws() %>%
      tolower(),
    group = case_when(
      active_clean == "true"  ~ "Intervention",
      active_clean == "false" ~ "Control",
      TRUE                    ~ NA_character_
    )
  ) %>%
  select(patient, group)


################################################################################
# Load PDQ-39 raw data
################################################################################

# Load data to workspace
## PDQ39
pdq39_raw <- read.csv2(
  file     = file.path(base_dir, "raw_data", "pdq39_sortiert_Final_dummy.withdropout.csv"), # "pdq39_sortiert_Final.csv"),
  encoding = "UTF-8"
)

# Only show outputs if sanity_check is TRUE
if (isTRUE(sanity_check)) {
  if (interactive()) utils::View(pdq39_raw, title = "Intervention descriptives")
}


########################################################
#join demographiep and moca for educational background
 
demographiep_path <- file.path(base_dir, "raw_data",
                               "cleaned_export_qform_5_2025-10-13-09-54_30_demapped.csv")

demographiep <- read.csv(demographiep_path)
unique(demographiep[["id30_Höchster.beruflicher.Abschluss"]])

ergänzungen <- read.csv2(
  file.path(base_dir, "raw_data", "schulbildung_ergänzt.csv")
)

moca <- read.csv(
  file.path(base_dir, "raw_data", "moca_final.csv")
)


moca <- moca %>%
  left_join(
    ergänzungen %>% select(patient, Wert),
    by = "patient"
  ) %>%
  mutate(
    extrapunkt_gesamt = coalesce(Wert, extrapunkt_gesamt)
  ) %>%
  select(-Wert)

################################################################################
# Load BDI, MoCA, UPDRS, Hoehn & Yahr
################################################################################

# bdi, moca, updrs, hoehnyahr
bdi_file       <- file.path(base_dir, "raw_data", "bdi_new.csv")
moca_file      <- file.path(base_dir, "raw_data", "moca_final.csv")
updrs_file     <- file.path(base_dir, "raw_data", "updrs_new.csv")
hoehnyahr_file <- file.path(base_dir, "raw_data", "hoehnyahr_new.csv")


read_scale <- function(path, exclude_patients) {
  read_csv(path, show_col_types = FALSE) %>%
    # filter(!patient %in% exclude_patients) %>%
    mutate(
      group     = factor(group,     levels = c("Control", "Intervention")),
      Timepoint = factor(Timepoint, levels = c("T0", "T6"))
    )
}

bdi       <- read_scale(bdi_file,       exclude_patients)
moca      <- read_scale(moca_file,      exclude_patients)
updrs     <- read_scale(updrs_file,     exclude_patients)
hoehnyahr <- read_scale(hoehnyahr_file, exclude_patients)


################################################################################
# MoCA variable renaming and selection
################################################################################

moca <- moca %>%
  rename(
    visuospatial_moca  = visuospatial_gesamt,
    naming_moca        = bennen_gesamt,
    attention_moca     = aufmerksamkeit_gesamt,
    language_moca      = sprache_gesamt,
    abstraction_moca   = abstraktion_gesamt,
    memory_moca        = erinnerung_gesamt,
    orientation_moca   = orientierung_gesamt,
    extra_points_moca  = extrapunkt_gesamt,
    total_score_moca   = Gesamtscore
  )

moca_vars <- c(
  "visuospatial_moca",
  "naming_moca",
  "attention_moca",
  "language_moca",
  "abstraction_moca",
  "memory_moca",
  "orientation_moca",
  "extra_points_moca",
  "total_score_moca"
)


################################################################################
# UPDRS variable renaming and selection
################################################################################

updrs <- updrs %>%
  rename(summaryindex_updrs = Summenindex)

updrs <- updrs %>%
  select(where(~ !all(is.na(.x))))

updrs_vars <- c("summaryindex_updrs", "Teil1", "Teil2", "Teil3", "Teil4")

################################################################################
# NMSS
################################################################################

nmss_raw = read.csv(file.path(base_dir, "raw_data", "cleaned_export_qform_5_2025-10-13-09-54_22_demapped.csv"))

# helper: extract first number from a character string
to_num <- function(x) {
  as.numeric(substr(trimws(x), 1, 1))
}

# find all Schwere-columns
schwere_idx  <- grep("Schwere$", names(nmss_raw))
schwere_cols <- names(nmss_raw)[schwere_idx]
name_cols <- names(nmss_raw)[schwere_idx-1]

iter = 0
for (col in schwere_cols) {
  iter = iter + 1
  i        <- match(col, names(nmss_raw))       # position of Schwere
  freq_col <- names(nmss_raw)[i + 1]            # next column = Häufigkeit
  
  schwere <- to_num(nmss_raw[[col]])
  haeuf   <- to_num(nmss_raw[[freq_col]])
  
  score <- dplyr::case_when(
    (!is.na(schwere) & schwere == 0) |
      (!is.na(haeuf) & haeuf == 0)         ~ 0,
    is.na(schwere) & is.na(haeuf)          ~ NA_real_,
    TRUE                                   ~ schwere * haeuf
  )
  
  nmss_raw[[name_cols[iter]]] <- score
}

nmss <- nmss_raw %>%
  select(
    # keep only columns that are NOT all NA
    where(~ !all(is.na(.x))),
    
    # and drop all columns that contain "Notizen" in their name
    -contains("Notizen")
  )

nmss_item_cols <- nmss %>%
  select(
    matches("^id[0-9]+_"),    # any id###_ (all NMSS items)
    -contains("Schwere"),
    -contains("Notizen"),
    -contains("Häufigkeit"),
    -starts_with("id153"),    # remove Ausgefüllt / Poststempeldatum
    -starts_with("id1_")      # remove Bereich 1 header
  ) %>%
  names()

nmss$nmss_total <- rowSums(select(nmss, all_of(nmss_item_cols)), na.rm = TRUE)


nmss <- nmss %>%
  rename(postdate = "id153_Ausgefüllt.am.....Poststempeldatum") %>%   # rename the column
  mutate(postdate = as.Date(postdate, format = "%d.%m.%Y"))          # convert to Date

nmss <- nmss %>%
  group_by(patient) %>%
  arrange(postdate, .by_group = TRUE) %>%
  mutate(
    Timepoint_num = dplyr::row_number() - 1,   # numeric order per patient
    Timepoint     = paste0("T", Timepoint_num) # e.g., T0, T1, T2 …
  ) %>%
  ungroup()

nmss <- nmss %>%
	mutate(Timepoint = recode(Timepoint, "T1" = "T6"))

nmss <- nmss %>%
  left_join(
    data_group_pat %>% rename(patient = patient_id),
    by = "patient"
  ) %>% 
  mutate(
    # robust mapping: handles "True"/"true"/1/yes → Intervention; else Control; NA stays NA
    group = case_when(
      is.na(active) ~ NA_character_,
      str_to_lower(active) %in% c("true", "1", "yes", "y") ~ "Intervention",
      TRUE ~ "Control"
    )
  ) %>%
  select(-active) %>%
  # keep only the important columns, in a tidy order
  select(patient, id, postdate, group, Timepoint, all_of(nmss_item_cols), nmss_total) %>%
  arrange(patient, Timepoint, postdate)


################################################################################
# eq5d
################################################################################

eq5d_raw = read.csv(file.path(base_dir, "raw_data", "cleaned_export_qform_5_2025-10-13-09-54_34_demapped.csv"))

eq5d <- eq5d_raw %>%
  rename(postdate = "id9_Ausgefüllt.am.....Poststempeldatum") %>%   # rename the column
  mutate(postdate = as.Date(postdate, format = "%d.%m.%Y"))          # convert to Date

eq5d <- eq5d %>%
	rename(likert = "id7_Ihre.Gesundheit.heute..Likertskala.")
	
eq5d_item_cols <- eq5d %>%
  select(
    matches("^id[0-9]+_"),    # any id###_ (all NMSS items)
    -contains("Schwere"),
    -contains("Notizen"),
    -starts_with("id1_")      # remove Bereich 1 header
  ) %>%
  names()

eq5d <- eq5d %>%
  group_by(patient) %>%
  arrange(postdate, .by_group = TRUE) %>%
  mutate(
    Timepoint_num = dplyr::row_number() - 1,   # numeric order per patient
    Timepoint     = paste0("T", Timepoint_num) # e.g., T0, T1, T2 …
  ) %>%
  ungroup()

eq5d <- eq5d %>%
	mutate(Timepoint = recode(Timepoint, "T1" = "T6"))

eq5d <- eq5d %>%
  left_join(
    data_group_pat %>% rename(patient = patient_id),
    by = "patient"
  ) %>% 
  mutate(
    # robust mapping: handles "True"/"true"/1/yes → Intervention; else Control; NA stays NA
    group = case_when(
      is.na(active) ~ NA_character_,
      str_to_lower(active) %in% c("true", "1", "yes", "y") ~ "Intervention",
      TRUE ~ "Control"
    )
  ) %>%
  select(-active) %>%
  # keep only the important columns, in a tidy order
  select(patient, id, postdate, group, Timepoint, all_of(eq5d_item_cols), likert) %>%
  arrange(patient, Timepoint, postdate)

# WHO-5

who5_raw = read.csv(file.path(base_dir, "raw_data", "export_who5_5_2025-12-08-10-00.csv"))

## Coding
#Die ganze Zeit: 5 Punkte
#Meistens: 4 Punkte
#Etwas mehr als die Hälfte der Zeit: 3 Punkte
#Etwas weniger als die Hälfte der Zeit: 2 Punkte
#Ab und zu: 1 Punkt
#Zu keinem Zeitpunkt: 0 Punkte

################################################################################
# Build master data (BDI, MoCA, UPDRS, Hoehn & Yahr)
################################################################################

master_data <- list(
  bdi       %>% select(patient, group, Timepoint, bdi_score),
  moca      %>% select(patient, group, Timepoint, all_of(moca_vars)),
  updrs     %>% select(patient, group, Timepoint, all_of(updrs_vars)),
  hoehnyahr %>% select(patient, group, Timepoint, hoehnyahrlevel),
  nmss      %>% select(patient, group, Timepoint, all_of(nmss_item_cols), nmss_total),
  eq5d 	    %>% select(patient, group, Timepoint, all_of(eq5d_item_cols))
) %>%
  reduce(full_join, by = c("patient", "group", "Timepoint")) %>%
  mutate(
    group_time = factor(
      paste(group, Timepoint, sep = "_"),
      levels = c("Control_T0", "Control_T6", "Intervention_T0", "Intervention_T6")
    )
  )


################################################################################
# Demographics with recoding and translation
################################################################################

demographics_pat <- read.csv(
  file     = file.path(base_dir, "raw_data", "cleaned_export_qform_5_2025-10-13-09-54_30_demapped.csv"),
  encoding = "UTF-8"
)

demographics_pat <- demographics_pat[-123, ] # somehow bulky way of removing duplicate

demographics_pat <- demographics_pat %>%
  left_join(group_data, by = "patient")

demographics_pat <- demographics_pat %>%
  left_join(master_data %>% filter(Timepoint == "T0"), by = "patient")

demographics_pat <- demographics_pat %>%
  left_join(randomisation_list , by = "patient")

demographics_pat <- demographics_pat %>%
  mutate(
    # Clean and translate marital status
    marital_status = id16_Familienstand %>%
      as.character() %>%       # make sure it's character
      str_squish() %>%         # trim and fix multiple spaces
      recode(
        "Geschieden/ getrennt" = "Divorced / separated",
        "Verwitwet" = "Widowed",
        "Verheiratet/ eingetragene Lebenspartnerschaft" = "Married / registered partnership",
        "Ledig/ unverheiratet" = "Single / unmarried"
      ) %>%
      # Set factor with a meaningful order
      factor(
        levels = c(
          "Single / unmarried",
          "Married / registered partnership",
          "Divorced / separated",
          "Widowed"
        )
      )
  )

# Only show outputs if sanity_check is TRUE
if (isTRUE(sanity_check)) {
  demographics_pat %>%
    count(group, marital_status, sort = FALSE) %>%
    print(n = Inf)
}


################################################################################
# Country of origin
################################################################################

country_map <- c(
  "Deutschland"     = "Germany",
  "Polen"           = "Poland",
  "Großbritannien"  = "United Kingdom",
  "England"         = "United Kingdom",
  "Österreich"      = "Austria",
  "Iran"            = "Iran",
  "Italien"         = "Italy"
)

demographics_pat <- demographics_pat %>%
  mutate(
    birth_country = id19_Geburtsland %>%
      as.character() %>%
      str_trim() %>%
      # treat common “no answer” codes as NA
      (\(x) replace(x, x %in% c("Keine Angabe", "k.A.", "NA", "n/a", "-", ".", "—", ""), NA_character_))() %>%
      # remove prefixes and trailing details
      str_remove("^Anderes \\(bitte nachfolgend angeben\\),\\s*") %>%
      str_remove("/.*$") %>%
      str_remove(",.*$") %>%
      str_trim() %>%
      # apply your standard country map
      recode(!!!country_map) %>%
      # final UK unification (handles unknown variants)
      (\(x) ifelse(
        str_detect(str_to_lower(x), "^england|gro(ß|ss)britannien|vereinig"),
        "United Kingdom",
        x
      ))() %>%
      factor()
  )


################################################################################
# Gender at birth
################################################################################

sex_map <- c(
  "Weiblich" = "Female",
  "Männlich" = "Male"
)

demographics_pat <- demographics_pat %>%
  mutate(
    sex_at_birth = id15_Geschlecht.bei.Geburt %>%
      as.character() %>%
      str_trim() %>%
      recode(!!!sex_map) %>%
      factor(levels = c("Female", "Male"))
  )


################################################################################
# Living situation
################################################################################

demographics_pat <- demographics_pat %>%
  mutate(
    housing = id17_Wohnsituation %>%
      as.character() %>%
      str_trim() %>%
      str_to_lower() %>%
      (\(x) case_when(
        is.na(x) ~ NA_character_,
        str_detect(x, "pflegeheim") ~ "Nursing home (qualified care)",
        str_detect(x, "betreut|seniorenheim|service wohnen") ~
          "Assisted living / senior residence",
        str_detect(x, "privater haushalt") & str_detect(x, "nicht allein") ~
          "Private household – not living alone",
        str_detect(x, "privater haushalt") & str_detect(x, "allein") &
          !str_detect(x, "nicht allein") ~
          "Private household – living alone",
        TRUE ~ "Other"
      ))() %>%
      factor(levels = c(
        "Private household – living alone",
        "Private household – not living alone",
        "Assisted living / senior residence",
        "Nursing home (qualified care)",
        "Other"
      ))
  )


################################################################################
# Language
################################################################################

demographics_pat <- demographics_pat %>%
  mutate(
    native_language = id53_Muttersprache %>%
      as.character() %>%
      str_trim() %>%
      str_to_lower() %>%
      # clean up "andere (...)" prefix
      str_remove("^andere\\s*\\([^)]*\\)\\s*,?\\s*") %>%
      # wrap in an anonymous function so case_when works cleanly
      (\(x) case_when(
        is.na(x) ~ NA_character_,
        str_detect(x, ",|/| und ") ~ "Multilingual",
        str_detect(x, "deutsch")   ~ "German",
        str_detect(x, "polnisch")  ~ "Polish",
        str_detect(x, "englisch")  ~ "English",
        str_detect(x, "italienisch") ~ "Italian",
        str_detect(x, "persisch")  ~ "Persian",
        str_detect(x, "^andere")   ~ "Other",
        TRUE ~ "Other"
      ))() %>%
      factor(levels = c(
        "German", "Polish", "English", "Persian", "Italian",
        "Other", "Multilingual"
      ))
  )


################################################################################
# Education
################################################################################

# Ordered final categories
levels_order <- c(
  "No degree",
  "Vocational school",
  "Technical school / master school",
  "Engineering school / polytechnic",
  "University / university of applied sciences",
  "Other degree"
)

# Regex definitions for each category
patterns <- c(
  "No degree"                                   = "kein abschluss",
  "Vocational school"                           = "berufsschule",
  "Technical school / master school"            = "fachschule|techniker|meister",
  "Engineering school / polytechnic"            = "ingenieur[- ]?schule|polytechnikum",
  "University / university of applied sciences" = "hochschule|universit[aä]t|fachhochschule|\\bfh\\b"
)

pick_highest <- function(s) {
  if (is.na(s)) return(NA_character_)

  hits <- vapply(
    patterns,
    function(p) str_detect(s, regex(p, ignore_case = TRUE)),
    logical(1)
  )

  if (any(hits)) {
    choices <- names(patterns)[hits]
    ranks   <- match(choices, levels_order)
    return(choices[which.max(ranks)])
  }

  if (str_detect(s, regex("sonstiger berufsabschluss", ignore_case = TRUE)))
    return("Other degree")

  return(NA_character_)
}

demographics_pat <- demographics_pat %>%
  mutate(
    education = id30_Höchster.beruflicher.Abschluss %>%
      as.character() %>%
      str_squish() %>%                      # clean spaces
      na_if("") %>%                         # empty -> NA
      na_if("nicht angegeben") %>%          # NA definition
      # normalize variants
      str_replace("Berufsschule \\(Lehre\\)", "Berufsschule") %>%
      str_replace("Fachschule/Techniker-/Meisterschule", "Fachschule Techniker Meister") %>%
      str_replace("Ingenieur-Schule/Polytechnikum", "Ingenieur-Schule Polytechnikum") %>%
      str_replace("Hochschule/Fachhochschule/Universität", "Hochschule Fachhochschule Universität") %>%
      # classify via pick_highest
      (\(x) vapply(x, pick_highest, character(1)))() %>%
      factor(levels = levels_order, ordered = TRUE)
  )


################################################################################
# Employment
################################################################################

demographics_pat <- demographics_pat %>%
  mutate(
    employment_status = case_when(
      str_detect(
        str_to_lower(str_squish(as.character(id34_Aktueller.Erwerbsstatus))),
        "angestellt"
      ) ~ "Employed",
      str_detect(
        str_to_lower(str_squish(as.character(id34_Aktueller.Erwerbsstatus))),
        "selbstständig"
      ) ~ "Self-employed",
      str_detect(
        str_to_lower(str_squish(as.character(id34_Aktueller.Erwerbsstatus))),
        "arbeitssuchend"
      ) ~ "Unemployed",
      str_detect(
        str_to_lower(str_squish(as.character(id34_Aktueller.Erwerbsstatus))),
        "andere"
      ) ~ "Other",
      TRUE ~ NA_character_
    ),
    employment_status = factor(
      employment_status,
      levels = c("Employed", "Self-employed", "Unemployed", "Other")
    )
  )


################################################################################
# Insurance
################################################################################

demographics_pat <- demographics_pat %>%
  mutate(
    insurance = .data[["id37_Name.der.Krankenversicherung"]] |> as.character(),
    insurance = str_replace_all(insurance, "[\\u00A0\\u2007\\u202F]", " "),
    insurance = str_squish(insurance),
    insurance = na_if(insurance, ""),
    insurance = case_when(
      is.na(insurance) ~ NA_character_,
      str_detect(insurance, regex("\\bAOK\\b", ignore_case = TRUE)) ~ "AOK",
      str_detect(insurance, regex("\\bBarmer\\b", ignore_case = TRUE)) ~ "Barmer",
      str_detect(insurance, regex("\\bDAK\\s*Gesundheit\\b", ignore_case = TRUE)) ~ "DAK Gesundheit",
      str_detect(insurance, regex("Kaufm[äa]nnische\\s+Krankenkasse|\\bKKH\\b", ignore_case = TRUE)) ~
        "Kaufmännische Krankenkasse (KKH)",
      str_detect(insurance, regex("Techniker\\s+Krankenkasse|\\bTK\\b", ignore_case = TRUE)) ~
        "Techniker Krankenkasse",
      str_detect(insurance, regex("\\bIKK\\s*Classic\\b", ignore_case = TRUE)) ~ "IKK Classic",
      str_detect(insurance, regex("\\bKnappschaft\\b", ignore_case = TRUE)) ~ "Knappschaft",
      TRUE ~ "Sonstige Krankenkasse"
    ),
    insurance = factor(
      insurance,
      levels = c(
        "AOK",
        "Barmer",
        "DAK Gesundheit",
        "Kaufmännische Krankenkasse (KKH)",
        "Techniker Krankenkasse",
        "IKK Classic",
        "Knappschaft",
        "Sonstige Krankenkasse"
      )
    )
  )


################################################################################
# Hoehn & Yahr
################################################################################

demographics_pat <- demographics_pat %>%
  mutate(
    hoehnyahr_t0 = recode(
      id3_Schweregrad.nach.Höhn...Yahr.Stadium.I.bis.IV,
      "false" = "No",
      "true"  = "Yes"
    ),
    hoehnyahr_t0 = factor(hoehnyahr_t0, levels = c("No", "Yes"))
  )


################################################################################
# Postal code / city
################################################################################

demographics_pat$city <- factor(trimws(demographics_pat[["id22_Postleitzahl"]]))
table(demographics_pat$city, useNA = "ifany")     # 13 NA

demographics_pat$city <- ifelse(
  is.na(demographics_pat$id22_Postleitzahl), NA,
  ifelse(demographics_pat$id22_Postleitzahl < 30000, "Hamburg", "Marburg")
)

demographics_pat$city <- factor(demographics_pat$city)
#TODO: there are 13 subjects without a postal code!


################################################################################
# Necessary changes and sanity checks in the raw PDQ-39 data
################################################################################


# This version is obsolete but should not be deleted; assuming that something went
# wrong when manually checking dta, so that  as.Date(postdate, format = "%d.%m.%Y")
# does not create meaningful date anymore.

pdq39_raw_old <- pdq39_raw %>%
  rename(postdate = "id85_Ausgefüllt.am.....Poststempeldatum") %>%   # rename the column
  mutate(postdate = as.Date(postdate, format = "%d.%m.%Y"))          # convert to Date

pdq39_raw <- pdq39_raw %>%
  rename(postdate_raw = "id85_Ausgefüllt.am.....Poststempeldatum") %>%
  mutate(
    postdate_raw = as.character(postdate_raw),   # ensure character
    postdate_raw = str_trim(postdate_raw),       # remove spaces
    postdate     = dmy(postdate_raw)             # convert dd.mm.yyyy → Date
  )
  
  
pdq39_raw <- pdq39_raw %>%
  group_by(patient) %>%
  arrange(as.Date(postdate), .by_group = TRUE) %>%
  mutate(
    Timepoint_num = dplyr::row_number() - 1,   # numeric order per patient
    Timepoint     = paste0("T", Timepoint_num) # e.g., T0, T1, T2 …
  ) %>%
  ungroup()

# Only perform this if sanity_check [see 01_preamble.R] is TRUE
if (isTRUE(sanity_check)) {

  # --- Parameters for date checks --------------------------------------------
  min_gap <- 15   # lower bound of allowed gap (days)
  max_gap <- 45   # upper bound of allowed gap (days)

  # --- Select relevant columns and prepare data ------------------------------
  df_small <- pdq39_raw %>%
    select(patient, Timepoint, postdate, id) %>%
    mutate(
      Timepoint = factor(Timepoint, levels = time_levels),
      postdate  = as.Date(postdate)
    ) %>%
    arrange(patient, postdate)  # compare time against *chronological* order

  # --- Calculate intervals and flags -----------------------------------------
  interval_checks <- df_small %>%
    mutate(
      tp_num = as.integer(sub("^T", "", as.character(Timepoint)))  # "T3" -> 3
    ) %>%
    group_by(patient) %>%
    arrange(postdate, .by_group = TRUE) %>%
    mutate(
      # Lags for comparisons
      prev_tp_num  = lag(tp_num),
      prev_tp_lab  = lag(Timepoint),
      prev_date    = lag(postdate),

      # Differences
      days_diff = as.numeric(postdate - prev_date, units = "days"),

      # Flags
      date_ok = is.na(days_diff) | dplyr::between(days_diff, min_gap, max_gap)
    ) %>%
    ungroup()

  # --- Detect an ID column if present ----------------------------------------
  id_candidates <- c("id", "ID", "Id", "patient_id", "subject", "Subject")

  # --- Rows with date-interval violations ------------------------------------
  violations_date <- interval_checks %>%
    filter(!date_ok) %>%
    transmute(
      patient        = .data$patient,
      !!id_col       := if (!is.na(id_col)) .data[[id_col]] else NULL,  # include ID if exists
      prev_Timepoint = prev_tp_lab,
      prev_date      = prev_date,
      Timepoint      = Timepoint,
      postdate       = postdate,
      days_diff      = days_diff
    )

  # --- Write CSV for date-interval violations --------------------------------
  out_path <- file.path(results_dir, "date_interval_violations.csv")
  readr::write_csv(violations_date, out_path)

  message(
    "✅ Wrote ", nrow(violations_date),
    " violation rows to: ", out_path
  )

  # --- Check for timestamp issues (T7–T10) -----------------------------------
  timestamp_checks <- df_small %>%
    filter(Timepoint %in% c("T7", "T8", "T9", "T10"))

  # --- Rows with timestamp violations ----------------------------------------
  violations_stamp <- interval_checks %>%
    filter(!date_ok) %>%
    transmute(
      patient        = .data$patient,
      !!id_col       := if (!is.na(id_col)) .data[[id_col]] else NULL,  # include ID if exists
      prev_Timepoint = prev_tp_lab,
      prev_date      = prev_date,
      Timepoint      = Timepoint,
      postdate       = postdate
    )

  # --- Write CSV for timestamp violations ------------------------------------
  out_path <- file.path(results_dir, "time_stamp_violations.csv")
  readr::write_csv(violations_stamp, out_path)
}

# view(pdq39_raw)

##########
# ?berpr?fen ES KLAPPPTT ERFOLG -> :DDD
##########


################################################################################
# Remap values for all PDQ-39 items
################################################################################

pdq_map <- c(
  "niemals"                        = 0,
  "selten"                         = 1,
  "manchmal"                       = 2,
  "häufig"                         = 3,
  "immer/kann ich überhaupt nicht" = 4
)

pdq39_raw <- pdq39_raw %>%
  mutate(across(
    8:54,
    ~ .x %>%
      as.character() %>%                # handles factors safely
      str_trim() %>%                    # remove leading/trailing spaces
      str_squish() %>%                  # collapse internal runs of spaces
      str_to_lower() %>%                # make matching case-insensitive
      na_if("") %>%                     # empty -> NA
      dplyr::recode(!!!pdq_map, .default = NA_real_, .missing = NA_real_)
  ))

# view(pdq39_raw)

write.csv(
  pdq39_raw,
  file.path(results_dir, "pdq39_raw.csv"),
  row.names   = FALSE,
  fileEncoding = "UTF-8"
)


################################################################################
# PDQ-39 domain scores (new version)
################################################################################

score_pct <- function(df, cols, item_max = 4) {
  vals  <- df[, cols, drop = FALSE]          # subset columns
  sums  <- rowSums(vals, na.rm = TRUE)       # sum of available items
  n_non <- rowSums(!is.na(vals))             # number answered
  denom <- n_non * item_max                  # max possible given n_non
  pct   <- 100 * sums / denom
  pct[denom == 0] <- NA_real_                # if nothing answered -> NA
  pct
}

pdq39_raw <- pdq39_raw %>%
  mutate(
    mobility_score            = score_pct(., 9:18),
    adl_score                 = score_pct(., 20:25),
    emotional_wellbeing_score = score_pct(., 27:32),
    stigma_score              = score_pct(., 34:37),
    social_support_score      = score_pct(., 39:41),
    cognition_score           = score_pct(., 43:46),
    communication_score       = score_pct(., 48:50),
    bodily_discomfort_score   = score_pct(., 52:54)
  )

################################################################################
# Sanity checks: missing domains per patient (optional)
################################################################################

if (isTRUE(sanity_check)) { # The next part relied heavily on a LLM so that credit goes to ChatAI!

  # 1) Define PDQ-39 domain column ranges
  domain_cols <- list(
    mobility            = 9:18,
    adl                 = 20:25,
    emotional_wellbeing = 27:32,
    stigma              = 34:37,
    social_support      = 39:41,
    cognition           = 43:46,
    communication       = 48:50,
    bodily_discomfort   = 52:54
  )

  # 2) Build expressions for mutate(): one flag per domain => *_all_missing
  flag_exprs <- imap(domain_cols, ~ expr(if_all(all_of(!!.x), is.na)))
  names(flag_exprs) <- paste0(names(domain_cols), "_all_missing")

  # 3) Add flags, build `missing_domains` per row, keep only rows with any all-missing domain
  pdq_missing <- pdq39_raw %>%
    mutate(!!!flag_exprs)

  flag_cols <- grep("_all_missing$", names(pdq_missing), value = TRUE)

  missing_cases <- pdq_missing %>%
    rowwise() %>%
    mutate(
      missing_domains = {
        nms <- names(which(c_across(all_of(flag_cols))))
        if (length(nms) == 0) NA_character_
        else gsub("_all_missing$", "", paste(nms, collapse = ", "))
      }
    ) %>%
    ungroup() %>%
    filter(if_any(all_of(flag_cols), identity)) %>%     # keep rows with any TRUE flag
    select(id, patient, Timepoint, postdate, all_of(flag_cols), missing_domains)

  results_dir <- file.path(base_dir, "results")
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

  out_path <- file.path(results_dir, "missing_domains_records.csv")
  write_csv(missing_cases, out_path)

  message("✅ Wrote ", nrow(missing_cases), " cases to: ", out_path)
}


################################################################################
# PDQ-39 sum index
################################################################################

pdq39_raw <- pdq39_raw %>%
  rowwise() %>%
  mutate(
    pdq39_sum_index = sum(c_across(62:69), na.rm = TRUE) / 8
  ) %>%
  ungroup()

# view(pdq39_raw)

test = TRUE	
if (isTRUE(test)){

pdq39_raw <- pdq39_raw %>%
  rowwise() %>%
  mutate(
    pdq39_sum_index =
      sum(c_across(62:69), na.rm = TRUE) /
      sum(!is.na(c_across(62:69)))
  ) %>%
  ungroup()

}

################################################################################
# Add group to all PDQ-39 values and export sorted data
################################################################################

sorted_pdq39 <- pdq39_raw %>%
  left_join(
    data_group_pat %>% rename(patient = patient_id),
    by = "patient"
  ) %>%
  mutate(
    # robust mapping: handles "True"/"true"/1/yes → Intervention; else Control; NA stays NA
    group = case_when(
      is.na(active) ~ NA_character_,
      str_to_lower(active) %in% c("true", "1", "yes", "y") ~ "Intervention",
      TRUE ~ "Control"
    )
  ) %>%
  select(-active) %>%
  # keep only the important columns, in a tidy order
  select(patient, id, postdate, pdq39_sum_index, Timepoint, Timepoint_num, group) %>%
  arrange(patient, Timepoint_num, postdate)

df <- sorted_pdq39
dummy_rows <- grepl("^Dummy", df$id)
df$pdq39_sum_index[dummy_rows] <- NA
df[dummy_rows & df$pdq39_sum_index == 0, c("patient", "id", "Timepoint")]
sorted_pdq39 <- df

# Optional: preview only when you want
if (isTRUE(sanity_check) && interactive()) {
  utils::View(sorted_pdq39, title = "sorted_pdq39")
}

# Add the centre of the intervention
sorted_pdq39 <- sorted_pdq39 %>%
  dplyr::left_join(
    demographics_pat %>% dplyr::select(patient, city),
    by = "patient"
  ) %>%
  left_join(
    randomisation_list %>% select(patient, age_years),
    by = "patient"
  )


# --- Save to a dynamic location under base_dir/results -----------------------
results_dir <- file.path(base_dir, "results")
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

readr::write_csv(sorted_pdq39, file.path(results_dir, "sorted_pdq39.csv"))

################################################################################
# Create TableOne
################################################################################

source("03_createTableOne.v1.0.R")

