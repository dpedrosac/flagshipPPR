install.packages(c("readr","dplyr","lubridate","janitor","tableone","stringr"))
install.packages("survival")
library(readr); library(dplyr); library(lubridate); library(janitor); library(tableone); library(survival); library(stringr)

file <- "C:/Users/akoel/OneDrive/Desktop/Dr. Arbeit/Tabeleone DemoP/cleaned_export_qform_5_2025-10-13-09-54_30_demapped.csv"
demographiep <- read.csv(file)
group_data <- read.csv("C:/Users/akoel/OneDrive/Desktop/Dr. Arbeit/I-K/export_patients_5_2025-10-13-09-52_demapped.csv")
View(group_data)
group_data <- group_data %>%
  rename(patient = patient_id) %>%
  mutate(active_clean = tolower(trimws(as.character(active)))) %>%
  mutate(group = ifelse(active_clean == "true", "Intervention",
                        ifelse(active_clean == "false", "Control", NA_character_))) %>%
  select(patient, group)

View(group_data)
demographiep <- demographiep %>%
  left_join(group_data, by = "patient")

table(demographiep$group, useNA = "ifany")
exclude_patients <- c(
  "ZL4A0VBU", "0TOLUW4TF", "AKUK06XMH", "MTPNPZCIY", "G1QP3RGZ5", "0TRNBST3B" , 
  "UP6W00S8", "A33LV19Y", "BNV3HYYX", "8VUZMN38",
  "2JS3X4L6", "8ZCQJQQW", "EX8AI06G", "I7UFJIW1",
  "IAA50B1K", "X8QUGGZ3", "J1TGCNP9"
)

demographiep <- demographiep %>%
  filter(!patient %in% exclude_patients)

View(demographiep)



#Familienstand
unique(demographiep[["id16_Familienstand"]])

demographiep <- demographiep %>%
  mutate(
    marital_status = recode(
      trimws(id16_Familienstand),
      "Geschieden/ getrennt" = "Divorced / separated",
      "Verwitwet" = "Widowed",
      "Verheiratet/ eingetragene Lebenspartnerschaft" = "Married / registered partnership",
      "Ledig/ unverheiratet" = "Single / unmarried"
    ),
    marital_status = factor(marital_status,
                            levels = c("Single / unmarried", "Married / registered partnership",
                                       "Divorced / separated", "Widowed"))
  )

table(demographiep$marital_status, useNA = "ifany")

library(tableone)

tab_marital_status <- CreateTableOne(
  vars       = c("marital_status"),
  strata = "group",
  data       = demographiep,
  factorVars = c("marital_status")
)

print(tab_marital_status, showAllLevels = TRUE, includeNA = TRUE,
      formatOptions = list(big.mark=".", decimal.mark=","))



#Umzug id18
library(tableone)
txt18 <- trimws(demographiep[["id18_Umzug.aufgrund.von.Gesundheit.innerhalb.der.letzten.12.Monate..z.B..in.eine.behindertengerechte.Wohnung..in.eine.Seniorenwohnanlage..in.ein.Altenheim.oder.in.ein.Pflegeheim.Pflegestation.im.Altenheim."]])
unique(demographiep[["id18_Umzug.aufgrund.von.Gesundheit.innerhalb.der.letzten.12.Monate..z.B..in.eine.behindertengerechte.Wohnung..in.eine.Seniorenwohnanlage..in.ein.Altenheim.oder.in.ein.Pflegeheim.Pflegestation.im.Altenheim."]])
demographiep$health_related_relocation_past12m <- case_when(
  is.na(txt18) ~ NA_character_,
  grepl("^ja", txt18, ignore.case = TRUE) ~ "Yes",
  grepl("^nein", txt18, ignore.case = TRUE) ~ "No",
  TRUE ~ NA_character_
)

demographiep$health_related_relocation_past12m <- factor(demographiep$health_related_relocation_past12m, levels = c("No", "Yes"))
table(demographiep$health_related_relocation_past12m, useNA = "ifany")
health_related_relocation_past12m_tab <- CreateTableOne(vars = c("marital_status","health_related_relocation_past12m"),
                                      strata = "group",
                                      data = demographiep,
                                      factorVars = c("marital_status","health_related_relocation_past12m"))
print(health_related_relocation_past12m_tab, showAllLevels = TRUE, includeNA = TRUE,
      formatOptions = list(big.mark=".", decimal.mark=","))


#geburtsland
demographiep$birth_country <- factor(trimws(demographiep[["id19_Geburtsland"]]))
unique(demographiep$birth_country)
bc <- trimws(demographiep[["id19_Geburtsland"]])
bc[bc %in% c("", "Keine Angabe", "k.A.", "NA", "n/a", "-", ".", "—")] <- NA
bc <- sub("^Anderes \\(bitte nachfolgend angeben\\),\\s*", "", bc) #löschen: Anderes und löschen: alles hinter "/" oder "," 
bc <- sub("/.*$","",bc) #polen/posen--> Polen
bc<- sub(",.*$","",bc) # österreich, wien--> Österreich
bc <- trimws(bc) #leerraum weg
lower<- tolower(bc)
bc[ grepl("^england|gro(ß|ss)britannien|vereinig", lower) ]

country_map <- c(
  "Deutschland" = "Germany",
  "Polen" = "Poland",
  "Großbritannien" = "United Kingdom",
  "England" = "United Kingdom",
  "Österreich" = "Austria",
  "Iran" = "Iran",
  "Italien" = "Italy"
)
bc_en <- recode(bc, !!!country_map)
demographiep$birth_country <- factor(bc_en)
table(demographiep$birth_country, useNA = "always")

tab_birth_country <- CreateTableOne(
  vars       = c("marital_status","health_related_relocation_past12m","birth_country"),
  strata = "group",
  data       = demographiep,
  factorVars = c("marital_status","health_related_relocation_past12m","birth_country")
)
print(tab_birth_country, showAllLevels = TRUE, includeNA = TRUE,
      formatOptions = list(big.mark=".", decimal.mark=","))



#Geschlecht bei Geburt
unique(demographiep[["id15_Geschlecht.bei.Geburt"]])
sex <- trimws(demographiep[["id15_Geschlecht.bei.Geburt"]])
sex_map <- c(
  "Weiblich" = "Female",
  "Männlich" = "Male"
)
sex_en <- recode(sex, !!!sex_map)
demographiep$sex_at_birth <- factor(sex_en, levels = c("Female", "Male"))

table(demographiep$sex_at_birth, useNA = "ifany")         #nurNA anzeigen, wenn exisitiert                          
library(tableone)
tab<- CreateTableOne(
  vars       = c("sex_at_birth","marital_status","health_related_relocation_past12m","birth_country"),
  strata = "group",
  data       = demographiep,
  factorVars = c("sex_at_birth","marital_status","health_related_relocation_past12m","birth_country")
) 
print(tab, showAllLevels = TRUE, includeNA = TRUE,
      formatOptions = list(big.mark=".", decimal.mark=","))

#Wohnsituation
unique(demographiep[["id17_Wohnsituation"]])
library(dplyr)
library(tableone)
w <- tolower(trimws(demographiep[["id17_Wohnsituation"]])) #tolower=klein; trimws=llerraum weg
demographiep$housing <- case_when(
  is.na(w) ~ NA_character_,                                   # NA bleibt NA
  grepl("pflegeheim", w) ~ "Nursing home (qualified care)",
  grepl("betreut|seniorenheim|service wohnen", w) ~ "Assisted living / senior residence",
  grepl("privater haushalt", w) & grepl("nicht allein", w) ~ "Private household – not living alone",
  grepl("privater haushalt", w) & grepl("allein", w) & !grepl("nicht allein", w) ~ "Private household – living alone",
  TRUE ~ "Other"                                             # nur übrige, nicht-NA Fälle
) |> factor(levels = c(
  "Private household – living alone",
  "Private household – not living alone",
  "Assisted living / senior residence",
  "Nursing home (qualified care)",
  "Other"
))
table(demographiep$housing, useNA = "ifany")
tab_housing <- CreateTableOne(
  vars       = c("sex_at_birth","housing","marital_status","health_related_relocation_past12m","birth_country"),
  strata = "group",
  data       = demographiep,
  factorVars = c("sex_at_birth","housing","marital_status","health_related_relocation_past12m","birth_country")
) 
print(tab_housing, showAllLevels = TRUE, includeNA = TRUE,
      formatOptions = list(big.mark=".", decimal.mark=","))

#Muttersprache 
unique(demographiep[["id53_Muttersprache"]])
s <-  tolower(trimws(demographiep[["id53_Muttersprache"]]))
s_core <- sub("^andere\\s*\\([^)]*\\)\\s*,?\\s*", "", s)#mehrsprachrig
demographiep$native_language <- case_when(
  is.na(s) ~ NA_character_,
  grepl(",|/| und ", s_core) ~ "Multilingual",
  grepl("deutsch", s) ~ "German",
  grepl("polnisch", s) ~ "Polish",
  grepl("englisch", s) ~ "English",
  grepl("italienisch", s) ~ "Italian",
  grepl("persisch", s) ~ "Persian",
  grepl("^andere", s) ~ "Other",
  TRUE ~ "Other"
) |> factor(levels = c(
  "German", "Polish", "English", "Persian", "Italian", "Other", "Multilingual"
))
table(demographiep$native_language, useNA = "ifany")

#mehrsprachig anzeigen
demographiep %>%
  filter(native_language == "Multilingual") %>%
  select(id53_Muttersprache)

tab_native_Language<- CreateTableOne(
  vars       = c("sex_at_birth","housing","health_related_relocation_past12m","birth_country","marital_status","native_language"),
  strata = "group",
  data       = demographiep,
  factorVars = c("sex_at_birth","housing","health_related_relocation_past12m","birth_country","marital_status","native_language")
) 
print(tab_native_Language, showAllLevels = TRUE, includeNA = TRUE,
      formatOptions = list(big.mark=".", decimal.mark=","))
summary(tab_native_Language)


#TEIL2

#höchster beruflicher Abschlusss
unique(demographiep[["id30_Höchster.beruflicher.Abschluss"]])

col_raw <- "id30_Höchster.beruflicher.Abschluss"

#rohwerte 
x <- demographiep[[col_raw]]
x <- trimws(gsub("\\s+", " ", x))  # doppelte Spaces reduzieren, trimmen
x[x == "" | grepl("^nicht angegeben$", x, ignore.case = TRUE)] <- NA #NA definiereb

#zielkategorie und rangfleg
levels_order <- c(
  "No degree",
  "Vocational school",
  "Technical school / master school",
  "Engineering school / polytechnic",
  "University / university of applied sciences",
  "Other degree"
)

#standardkategoriem
x <- gsub("Berufsschule \\(Lehre\\)", "Berufsschule", x)
x <- gsub("Fachschule/Techniker-/Meisterschule", "Fachschule Techniker Meister", x)
x <- gsub("Ingenieur-Schule/Polytechnikum", "Ingenieur-Schule Polytechnikum", x)
x <- gsub("Hochschule/Fachhochschule/Universität", "Hochschule Fachhochschule Universität", x)
x <- gsub("Sonstiger Berufsabschluss", "Sonstiger Berufsabschluss", x)
patterns <- c(
  "No degree"                      = "kein Abschluss",
  "Vocational school"                = "Berufsschule",
  "Technical school / master school" = "Fachschule|Techniker|Meister",
  "Engineering school / polytechnic"      = "Ingenieur-?Schule|Polytechnikum",
  "University / university of applied sciences" =
    "Hochschule|Universit[aä]t|Fachhochschule|\\bFH\\b"
)

#bei mehreren ntworten, die höchste wählen
pick_highest <- function(s) {
  if (is.na(s)) return(NA_character_)
  
  hits <- vapply(patterns, function(p) grepl(p, s, ignore.case = TRUE), logical(1)) #logial=true oder false, vapply= nacheinander alles von patterns alsp
  
  if (any(hits)) {
    choices   <- names(patterns)[hits]             # alle erkannten Kategorien
    levels_ix <- match(choices, levels_order)      # Position in der Rangfolge
    return(choices[which.max(levels_ix)])          # höchste zurückgeben
  }
  
  #keine Standardkategorie, aber Sonstiger da 
  if (grepl("Sonstiger Berufsabschluss", s, ignore.case = TRUE))
    return("Other degree")
  
  #NA
  return(NA_character_)
}

#anwenden, faktor erzeugen
demographiep_education <- vapply(x, pick_highest, character(1))
demographiep_education <- factor(demographiep_education,
                                 levels = levels_order,
                                 ordered = TRUE)


print(addmargins(table(demographiep_education, useNA = "ifany")))

demographiep$education <- demographiep_education

tab_education <- CreateTableOne(
  vars       = c("sex_at_birth","housing","health_related_relocation_past12m","birth_country","marital_status","native_language","education"),
  strata = "group",
  data       = demographiep,
  factorVars = c("sex_at_birth","housing","health_related_relocation_past12m","birth_country","marital_status","native_language","education")
)

print(tab_education,
      showAllLevels = TRUE,
      missing = TRUE,
      formatOptions = list(big.mark = ".", decimal.mark = ",")
)

#erwerbstätig
unique(demographiep[["id33_Sind.Sie.noch.erwerbstätig."]])
col_emp <- "id33_Sind.Sie.noch.erwerbstätig."
x_emp <- trimws(demographiep[[col_emp]]) #leerzeichen
x_emp[x_emp == "" | is.na(x_emp)] <- NA #NA

emp_cat <- dplyr::case_when(
  grepl("^true$",  x_emp, ignore.case = TRUE) ~ "Yes",
  grepl("^false$", x_emp, ignore.case = TRUE) ~ "No",
  TRUE ~ NA_character_
)
demographiep$currently_employed <- factor(emp_cat, levels = c("No","Yes"), ordered = TRUE)
table(demographiep$currently_employed, useNA = "ifany")
tab_employment <- CreateTableOne(
  vars       = c("sex_at_birth","housing","health_related_relocation_past12m",
                 "birth_country","marital_status","native_language","education",
                 "currently_employed"
                  ),
  strata = "group",
  data       = demographiep,
  factorVars = c("sex_at_birth","housing","health_related_relocation_past12m",
                 "birth_country","marital_status","native_language","education",
                 "currently_employed")
)
print(tab_employment,
      showAllLevels = TRUE,
      missing = TRUE,
      formatOptions = list(big.mark=".", decimal.mark=","))
#erwerbsstatus
unique(demographiep[["id34_Aktueller.Erwerbsstatus"]])

demographiep <- demographiep %>%
  mutate(id34_clean = na_if(trimws(id34_Aktueller.Erwerbsstatus), ""))

demographiep$employment_status <- NA_character_
demographiep$employment_status[grepl("Angestellt",     demographiep$id34_clean, ignore.case = TRUE)] <- "Employed"
demographiep$employment_status[grepl("Selbstständig",  demographiep$id34_clean, ignore.case = TRUE)] <- "Self-employed"
demographiep$employment_status[grepl("Arbeitssuchend", demographiep$id34_clean, ignore.case = TRUE)] <- "Unemployed"
demographiep$employment_status[grepl("Andere",         demographiep$id34_clean, ignore.case = TRUE)] <- "Other"
demographiep$employment_status <- factor(
  demographiep$employment_status,
  levels = c("Employed", "Self-employed", "Unemployed", "Other")
)
table(demographiep$employment_status, useNA = "ifany")

tab_employment_status <- CreateTableOne(
  vars       = c("sex_at_birth","housing","health_related_relocation_past12m",
                 "birth_country","marital_status","native_language","education",
                 "currently_employed","employment_status"),
  strata = "group",
  data       = demographiep,
  factorVars = c("sex_at_birth","housing","health_related_relocation_past12m",
                 "birth_country","marital_status","native_language","education",
                 "currently_employed", "employment_status")
)
print(tab_employment_status,
      showAllLevels = TRUE,
      missing = TRUE,
      formatOptions = list(big.mark=".", decimal.mark=","))

#Erkrankung und erwerbsstatus
unique(demographiep[["id39_Hat.Ihre.Erkrankung.etwas.an.Ihrem.Erwerbsstatus.geändert."]])
id40 <- "id40_Falls.Ja..inwiefern.hat.Ihre.Erkrankung.etwas.an.Ihrer.beruflichen.Tätigkeit.verändert."
demographiep <- demographiep %>%
  mutate(
    changed_employment = recode(
      id39_Hat.Ihre.Erkrankung.etwas.an.Ihrem.Erwerbsstatus.geändert.,
      "true"  = "Yes",
      "false" = "No"
    ),
    changed_employment = factor(changed_employment, levels = c("No", "Yes"))
  )
demographiep <- demographiep %>%
  mutate(
    id40_raw = if_else(changed_employment == "Yes",
                       as.character(id40_Falls.Ja..inwiefern.hat.Ihre.Erkrankung.etwas.an.Ihrer.beruflichen.Tätigkeit.verändert.),
                       NA_character_),
    id40_raw = str_replace_all(id40_raw, "[\\u00A0\\u2007\\u202F]", " "),
    id40_raw = str_squish(id40_raw),
    id40_raw = na_if(id40_raw, ""),
    employment_change_type = case_when(
      is.na(id40_raw) ~ NA_character_,
      str_detect(id40_raw, regex("niedergelegt", ignore_case = TRUE)) ~ "Stopped working completely",
      str_detect(id40_raw, regex("verändert/?\\s*angepasst", ignore_case = TRUE)) ~ "Changed/adapted job",
      str_detect(id40_raw, regex("Arbeitszeit reduziert", ignore_case = TRUE)) ~ "Reduced working hours",
      TRUE ~ "Other change"
    ),
    employment_change_type = factor(
      employment_change_type,
      levels = c("Stopped working completely", "Changed/adapted job", "Reduced working hours", "Other change")
    )
  )
tab_employment <- CreateTableOne(
  vars = c("sex_at_birth","housing","health_related_relocation_past12m",
           "birth_country","marital_status","native_language","education",
           "currently_employed", "employment_status","changed_employment", "employment_change_type"),
  strata = "group",
  data = demographiep,
  factorVars = c("sex_at_birth","housing","health_related_relocation_past12m",
                 "birth_country","marital_status","native_language","education",
                 "currently_employed", "employment_status", "changed_employment",
                 "employment_change_type")
)

print(tab_employment,
      showAllLevels = TRUE,
      missing = TRUE,
      formatOptions = list(big.mark = ".", decimal.mark = ","))

#KV
unique(demographiep[["id37_Name.der.Krankenversicherung"]])
id37 <- "id37_Name.der.Krankenversicherung"

demographiep <- demographiep %>%
  mutate(
    insurance = as.character(id37_Name.der.Krankenversicherung),
    insurance = str_replace_all(insurance, "[\\u00A0\\u2007\\u202F]", " "),
    insurance = str_squish(insurance),
    insurance = na_if(insurance, ""),
    insurance = case_when( #prio
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
table(demographiep$id37_kat, useNA = "ifany")

insurance_tab <- CreateTableOne(
  vars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance"          # <- neue Variable
  ),
  strata = "group",
  data = demographiep,
  factorVars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance"
  )
)

print(
  insurance_tab,
  showAllLevels = TRUE,
  missing = TRUE,
  formatOptions = list(big.mark=".", decimal.mark=",")
)

#Stürze
unique(demographiep[["id56_Wiederkehrende.Stürze.vorhanden."]])
x_falls <- trimws(demographiep[["id56_Wiederkehrende.Stürze.vorhanden."]])
demographiep <- demographiep %>%
  mutate(
    recurrent_falls = factor(
      case_when(
        grepl("^true$",  x_falls, ignore.case = TRUE)  ~ "Yes",
        grepl("^false$", x_falls, ignore.case = TRUE)  ~ "No",
        TRUE ~ NA_character_
      ),
      levels = c("No","Yes")
    )
  )
table(demographiep$recurrent_falls, useNA = "ifany")

falls_tab <- CreateTableOne(
  vars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls"         
  ),
  strata = "group",
  data = demographiep,
  factorVars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls"
  )
)

print(
  falls_tab,
  showAllLevels = TRUE,
  missing = TRUE,
  formatOptions = list(big.mark=".", decimal.mark=",")
)

#stürze Jahre ja,n=30--> Kategorie da wshl median statistisch instabil
library(stringr)
View(demographiep)
unique(demographiep[["id62_Wie.lange.sind.die.Stürze.schon.präsent...Jahre."]])
demographiep %>%
  filter(grepl("seit diesem Jahr", id62_Wie.lange.sind.die.Stürze.schon.präsent...Jahre.))

parse_years_de <- function(x) {
  x0 <- trimws(x)
  if (is.na(x0) || x0 == "") return(NA_real_)
  x1 <- tolower(x0)
  x1 <- str_squish(chartr(",", ".", x1))
  
  # direkt als 2 Jahre setzen
  if (str_detect(x1, "seit diesem jahr")) return(2)
  if (str_detect(x1, "januar 2024")) return(2)
  #zuordnen3-4,3 bus 4 und mittelwert
  if (str_detect(x1, "\\d+\\s*(?:-|–|bis)\\s*\\d+")) {
    nums <- str_match_all(x1, "(\\d+(?:\\.\\d+)?)")[[1]][,2]
    return(mean(as.numeric(nums[1:2])))
  }
  m <- str_match(x1, "^(\\d+(?:\\.\\d+)?)\\s*(jahr|jahre)?$")
  if (!is.na(m[1,2])) return(as.numeric(m[1,2]))
  return(NA_real_)
}
#auf den datensatz, schwierig, weil NA sich auch auf No bezieht, weiß nicht wie 
x62 <- demographiep[["id62_Wie.lange.sind.die.Stürze.schon.präsent...Jahre."]]
years62 <- vapply(x62, parse_years_de, numeric(1))
demographiep <- demographiep %>%
  mutate(
    falls_duration_years = ifelse(recurrent_falls == "Yes", years62, NA_real_),   #nur uf ja
    falls_duration_years = case_when( 
      recurrent_falls != "Yes" ~ NA_character_,  #kategroein bilden
      is.na(falls_duration_years) ~ NA_character_,
      falls_duration_years < 1    ~ "<1 Year",
      falls_duration_years < 3    ~ "1–<3 Years",
      falls_duration_years < 5    ~ "3–<5 Years",
      TRUE              ~ "≥5 Years"
    )
  )
table(demographiep$falls_duration_years, useNA = "ifany")
#levels
demographiep <- demographiep %>%
  mutate(
    falls_duration_years = factor(
      falls_duration_years,
      levels = c("<1 Year", "1–<3 Years", "3–<5 Years", "≥5 Years")
    )
  )
table(demographiep$falls_duration_years, useNA = "ifany")



years_falls_tab<- CreateTableOne(
    vars = c(
      "sex_at_birth","housing","health_related_relocation_past12m",
      "birth_country","marital_status","native_language","education",
      "currently_employed", "employment_status", "changed_employment",
      "employment_change_type","insurance","recurrent_falls","falls_duration_years"         
    ),
    strata = "group",
    data = demographiep,
    factorVars = c(
      "sex_at_birth","housing","health_related_relocation_past12m",
      "birth_country","marital_status","native_language","education",
      "currently_employed", "employment_status", "changed_employment",
      "employment_change_type","insurance","recurrent_falls","falls_duration_years"
    )
  )
print(years_falls_tab, showAllLevels = TRUE, missing = TRUE,
      catDigits = 1,
      formatOptions = list(big.mark=".", decimal.mark=","))


#PS diagnose
unique(demographiep[["id2_Klinische.Diagnose.eines.idiopathischen.Parkinson.Syndroms..iPS..vorliegend"]])
demographiep <- demographiep %>%
  mutate(
    clinical_diagnosis_ips = recode(
      id2_Klinische.Diagnose.eines.idiopathischen.Parkinson.Syndroms..iPS..vorliegend,
      "false" = "No",
      "true"  = "Yes"
    ),
    clinical_diagnosis_ips = factor(clinical_diagnosis_ips, levels = c("No", "Yes"))
  )

clinical_diagnosis_ips_tab <- CreateTableOne(
  vars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls","falls_duration_years","clinical_diagnosis_ips"
  ),
  strata = "group",
  data = demographiep,
  factorVars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls","falls_duration_years","clinical_diagnosis_ips"
  )
)
print(
  clinical_diagnosis_ips_tab,
  showAllLevels = TRUE,
  missing = TRUE,
  formatOptions = list(big.mark=".", decimal.mark=",")
)

#hoehnYahr
unique(demographiep[["id3_Schweregrad.nach.Höhn...Yahr.Stadium.I.bis.IV"]])
demographiep <- demographiep %>%
  mutate(
    hoehnyahr_t0 = recode(
      id3_Schweregrad.nach.Höhn...Yahr.Stadium.I.bis.IV,
      "false" = "No",
      "true"  = "Yes"
    ),
    hoehnyahr_t0 = factor(hoehnyahr_t0, levels = c("No", "Yes"))
  )
hoehnyahr_t0_tab <- CreateTableOne(
  vars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls","falls_duration_years","clinical_diagnosis_ips", "hoehnyahr_t0"
  ),
  strata = "group",
  data = demographiep,
  factorVars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls","falls_duration_years","clinical_diagnosis_ips", "hoehnyahr_t0"
) 
)
print(
  hoehnyahr_t0_tab,
  showAllLevels = TRUE,
  missing = TRUE,
  formatOptions = list(big.mark=".", decimal.mark=",")
)
#>18
unique(demographiep[["id4_Älter.als.18.Jahre"]])
demographiep <- demographiep %>%
  mutate(
    age_over18 = recode(
      id4_Älter.als.18.Jahre,
      "false" = "No",
      "true"  = "Yes"
    ),
    age_over18 = factor(age_over18, levels = c("No", "Yes"))
  )
age_over18_tab <- CreateTableOne(
  vars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls","falls_duration_years",
    "clinical_diagnosis_ips", "hoehnyahr_t0", "age_over18"
  ), 
  strata = "group",
  data = demographiep,
  factorVars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls","falls_duration_years",
    "clinical_diagnosis_ips", "hoehnyahr_t0", "age_over18"
  )
)
print(
  age_over18_tab,
  showAllLevels = TRUE,
  missing = TRUE,
  formatOptions = list(big.mark=".", decimal.mark=",")
)
#deutschkenntnisse id5_Gute.Deutschkenntnisse
unique(demographiep[["id5_Gute.Deutschkenntnisse"]])
demographiep <- demographiep %>%
  mutate(
    german_knowledge = recode(
      id5_Gute.Deutschkenntnisse,
      "false" = "No",
      "true"  = "Yes"
    ),
    german_knowledge = factor(german_knowledge, levels = c("No", "Yes"))
  )
german_knowledge_tab <- CreateTableOne(
  vars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls","falls_duration_years",
    "clinical_diagnosis_ips", "hoehnyahr_t0", "age_over18", "german_knowledge"
  ),
  strata = "group",
  data = demographiep,
  factorVars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls","falls_duration_years",
    "clinical_diagnosis_ips", "hoehnyahr_t0", "age_over18", "german_knowledge"
  )
)
print(
  german_knowledge_tab,
  showAllLevels = TRUE,
  missing = TRUE,
  formatOptions = list(big.mark=".", decimal.mark=",")
)
#einwilligung "id6_Einwilligungsfähig.und..bereit"
unique(demographiep[["id6_Einwilligungsfähig.und..bereit"]])
demographiep <- demographiep %>%
  mutate(
    capable_consent = recode(
      id6_Einwilligungsfähig.und..bereit,
      "false" = "No",
      "true"  = "Yes"
    ),
    capable_consent = factor(capable_consent, levels = c("No", "Yes"))
  )
capable_consent_tab <- CreateTableOne(
  vars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls","falls_duration_years",
    "clinical_diagnosis_ips", "hoehnyahr_t0", "age_over18", "german_knowledge",
    "capable_consent"
  ),
  strata = "group",
  data = demographiep,
  factorVars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls","falls_duration_years",
    "clinical_diagnosis_ips", "hoehnyahr_t0", "age_over18", "german_knowledge",
    "capable_consent"
  )
)
print(
  consent_tab,
  showAllLevels = TRUE,
  missing = TRUE,
  formatOptions = list(big.mark=".", decimal.mark=",")
)
#wlan "id8_WLAN.Verbindung.zuhause", 
unique(demographiep[["id8_WLAN.Verbindung.zuhause"]])
demographiep <- demographiep %>%
  mutate(
    wifi_at_home = recode(
      id6_Einwilligungsfähig.und..bereit,
      "false" = "No",
      "true"  = "Yes"
    ),
    wifi_at_home = factor(wifi_at_home, levels = c("No", "Yes"))
  )
wifi_at_home_tab <- CreateTableOne(
  vars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls","falls_duration_years",
    "clinical_diagnosis_ips", "hoehnyahr_t0", "age_over18", "german_knowledge",
    "capable_consent","wifi_at_home"
  ),
  strata = "group",
  data = demographiep,
  factorVars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls","falls_duration_years",
    "clinical_diagnosis_ips", "hoehnyahr_t0", "age_over18", "german_knowledge",
    "capable_consent","wifi_at_home"
  )
)
print(
  wifi_at_home_tab,
  showAllLevels = TRUE,
  missing = TRUE,
  formatOptions = list(big.mark=".", decimal.mark=",")
)

#kognition
unique(demographiep[["id9_Vorhandensein.einer..die.Kognition.beeinträchtigenden.Erkrankung..z.B..schwere.Demenz..Tumore.etc.."]])
demographiep <- demographiep %>%
  mutate(
    cognitive_impairment = recode(
      id9_Vorhandensein.einer..die.Kognition.beeinträchtigenden.Erkrankung..z.B..schwere.Demenz..Tumore.etc..,
      "false" = "No",
      "true"  = "Yes"
    ),
    cognitive_impairment = factor(cognitive_impairment, levels = c("No", "Yes"))
  )
cognitive_impairment_tab <- CreateTableOne(
  vars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls","falls_duration_years",
    "clinical_diagnosis_ips", "hoehnyahr_t0", "age_over18", "german_knowledge",
    "capable_consent","wifi_at_home","cognitive_impairment"
  ),
  strata = "group",
  data = demographiep,
  factorVars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls","falls_duration_years",
    "clinical_diagnosis_ips", "hoehnyahr_t0", "age_over18", "german_knowledge",
    "capable_consent","wifi_at_home","cognitive_impairment"
  )
)
print(
  cognitive_impairment_tab,
  showAllLevels = TRUE,
  missing = TRUE,
  formatOptions = list(big.mark=".", decimal.mark=",")
)
#Pflegepartner
unique(demographiep[["id13_Pflegepartner.in..Angehörigen.beabsichtig.eine.Teilnahme."]])
demographiep <- demographiep %>%
  mutate(
    carepartner_raw = str_squish(as.character(`id13_Pflegepartner.in..Angehörigen.beabsichtig.eine.Teilnahme.`)),
    carepartner_intends_participation = case_when(
      carepartner_raw == ""               ~ NA_character_,
      carepartner_raw == "N/Z"            ~ NA_character_,        # falls „nicht zutreffend“/fehlend
      str_detect(carepartner_raw, regex("^ja",   ignore_case=TRUE)) ~ "Yes",
      str_detect(carepartner_raw, regex("^nein", ignore_case=TRUE)) ~ "No",
      TRUE ~ NA_character_
    ),
    carepartner_intends_participation = factor(carepartner_intends_participation, levels = c("No","Yes"))
  )
unique(demographiep$carepartner_raw)
table(demographiep$carepartner_participation, useNA = "ifany")

carepartner_intends_participation_tab <- CreateTableOne(
  vars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls","falls_duration_years",
    "clinical_diagnosis_ips", "hoehnyahr_t0", "age_over18", "german_knowledge",
    "capable_consent","wifi_at_home","cognitive_impairment","carepartner_intends_participation"
    ), 
  strata = "group",
  data = demographiep,
  factorVars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls","falls_duration_years",
    "clinical_diagnosis_ips", "hoehnyahr_t0", "age_over18", "german_knowledge",
    "capable_consent","wifi_at_home","cognitive_impairment","carepartner_intends_participation"
    )
)

print(
  carepartner_intends_participation_tab,
  showAllLevels = TRUE,
  missing = TRUE,
  formatOptions = list(big.mark=".", decimal.mark=",")
)
#kommunikationsschwierigkeiten
unique(demographiep[["id11_Sonstige.Verständnis..und.Kommunikationsschwierigkeiten.welche.die.Fähigkeit.zur.Entscheidungsfindung.und..kommunikation.beeinträchtigen..z.B..Sprachprobleme."]])
demographiep <- demographiep %>%
  mutate(
    communication_difficulties = recode(
      id11_Sonstige.Verständnis..und.Kommunikationsschwierigkeiten.welche.die.Fähigkeit.zur.Entscheidungsfindung.und..kommunikation.beeinträchtigen..z.B..Sprachprobleme.,
      "false" = "No",
      "true"  = "Yes"
    ),
    communication_difficulties = factor(communication_difficulties, levels = c("No", "Yes"))
  )
communication_difficulties_tab <- CreateTableOne(
  vars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls","falls_duration_years",
    "clinical_diagnosis_ips", "hoehnyahr_t0", "age_over18", "german_knowledge",
    "capable_consent","wifi_at_home","cognitive_impairment","carepartner_intends_participation",
    "communication_difficulties"
  ),
  strata = "group",
  data = demographiep,
  factorVars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls","falls_duration_years",
    "clinical_diagnosis_ips", "hoehnyahr_t0", "age_over18", "german_knowledge",
    "capable_consent","wifi_at_home","cognitive_impairment","carepartner_intends_participation",
    "communication_difficulties"
  )
)
print(
  communication_difficulties_tab,
  showAllLevels = TRUE,
  missing = TRUE,
  formatOptions = list(big.mark=".", decimal.mark=",")
)
##postleitzahl
colnames(demographiep)
unique(demographiep[["id22_Postleitzahl"]])
demographiep$city <- factor(trimws(demographiep[["id22_Postleitzahl"]]))
table(demographiep$city, useNA = "ifany")     #13 NA
demographiep$city <- ifelse(
  is.na(demographiep$id22_Postleitzahl), NA,
  ifelse(demographiep$id22_Postleitzahl < 30000, "Hamburg", "Marburg")
)
demographiep$city <- factor(demographiep$city)
table(demographiep$city, useNA = "ifany") 

tab_city <- CreateTableOne(
  vars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls","falls_duration_years",
    "clinical_diagnosis_ips", "hoehnyahr_t0", "age_over18", "german_knowledge",
    "capable_consent","wifi_at_home","cognitive_impairment","carepartner_intends_participation",
    "communication_difficulties","city"
  ),
  strata = "group",
  data = demographiep,
  factorVars = c(
    "sex_at_birth","housing","health_related_relocation_past12m",
    "birth_country","marital_status","native_language","education",
    "currently_employed", "employment_status", "changed_employment",
    "employment_change_type","insurance","recurrent_falls","falls_duration_years",
    "clinical_diagnosis_ips", "hoehnyahr_t0", "age_over18", "german_knowledge",
    "capable_consent","wifi_at_home","cognitive_impairment","carepartner_intends_participation",
    "communication_difficulties", "city"
  )
)
print(
  tab_city,
  showAllLevels = TRUE,
  missing = TRUE,
  formatOptions = list(big.mark=".", decimal.mark=",")
)

mat <- print(
  tab_city,
  showAllLevels = TRUE,
  missing = TRUE,
  printToggle = FALSE,
  formatOptions = list(big.mark = ".", decimal.mark = ",")
)

df_tab_city <- as.data.frame(mat, stringsAsFactors = FALSE)
df_tab_city <- tibble::rownames_to_column(df_tab_city, "Variable")

write.csv(df_tab_city, "C:/Users/akoel/OneDrive/Desktop/Dr. Arbeit/descriptives.R/demographics_tableone.csv", row.names = FALSE)


#bdi,moca,updrs,hoehnyahr
bdi_file <- "C:/Users/akoel/OneDrive/Desktop/Dr. Arbeit/Fragebogen/BDI/bdi_new.csv"
moca_file <- "C:/Users/akoel/OneDrive/Desktop/Dr. Arbeit/Fragebogen/MoCA/moca_final.csv"
updrs_file <- "C:/Users/akoel/OneDrive/Desktop/Dr. Arbeit/Fragebogen/updrs/updrs_new.csv"
hoehnyahr_file <- "C:\\Users\\akoel\\OneDrive\\Desktop\\Dr. Arbeit\\Fragebogen\\HoehnYahr\\hoehnyahr_new.csv"

exclude_patients <- c(
  "ZL4A0VBU", "0TOLUW4TF", "AKUK06XMH", "MTPNPZCIY", "G1QP3RGZ5", "0TRNBST3B" , 
  "UP6W00S8", "A33LV19Y", "BNV3HYYX", "8VUZMN38",
  "2JS3X4L6", "8ZCQJQQW", "EX8AI06G", "I7UFJIW1",
  "IAA50B1K", "X8QUGGZ3", "J1TGCNP9"
)

bdi <- read.csv(bdi_file) %>%
  filter(!patient %in% exclude_patients) %>%
  mutate(
    group = factor(group, levels = c("Control", "Intervention")),
    Timepoint = factor(Timepoint, levels = c("T0", "T6"))
  )
moca <- read.csv(moca_file) %>%
  filter(!patient %in% exclude_patients) %>%
  mutate(
    group = factor(group, levels = c("Control", "Intervention")),
    Timepoint = factor(Timepoint, levels = c("T0", "T6"))
  )
updrs <- read.csv(updrs_file) %>%
  filter(!patient %in% exclude_patients) %>%
  mutate(
    group = factor(group, levels = c("Control", "Intervention")),
    Timepoint = factor(Timepoint, levels = c("T0", "T6"))
  )
unique(updrs[["id3_Primäre.Informationsquelle."]])
hoehnyahr <- read.csv(hoehnyahr_file) %>%
  filter(!patient %in% exclude_patients) %>%
  mutate(
    group = factor(group, levels = c("Control", "Intervention")),
    Timepoint = factor(Timepoint, levels = c("T0", "T6"))
  )

#variable bdi,moca,updrs,hoehnyahr
###bdi
bdi_vars <- c("bdi_score")

hoehnyahr$hoehnyahrlevel <- factor(hoehnyahr$hoehnyahrlevel, levels = c("I", "II", "III", "IV", "V"))
hoehnyahr_vars <- c("hoehnyahrlevel")
###moca
moca <- moca %>%
  rename(visuospatial_moca = visuospatial_gesamt,
         naming_moca = bennen_gesamt,
         attention_moca = aufmerksamkeit_gesamt,
         language_moca = sprache_gesamt,
         abstraction_moca = abstraktion_gesamt,
         memory_moca = erinnerung_gesamt,
         orientation_moca = orientierung_gesamt,
         extra_points_moca = extrapunkt_gesamt,
         total_score_moca = Gesamtscore   )
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

### updrs

updrs <- updrs %>%
  rename(summaryindex_updrs = Summenindex)

updrs <- updrs %>%
  rename(primary_information_source = id3_Primäre.Informationsquelle.)

updrs$primary_information_source <- ifelse(
  updrs$primary_information_source == "",
  NA,
  updrs$primary_information_source
)
updrs$primary_information_source <- factor(
  updrs$primary_information_source,
  levels = c("Patient", "Betreuungsperson"),  
  labels = c("Patient", "Caregiver")
)

unique(updrs[["id36_3a.Erhält.der.Patient.Medikamente.zur.Behandlung."]])
updrs <- updrs %>%
  rename(intake_of_medication = id36_3a.Erhält.der.Patient.Medikamente.zur.Behandlung.)
updrs$intake_of_medication <- factor(
  ifelse(
    updrs$intake_of_medication == "true", "Yes",
    ifelse(
      updrs$intake_of_medication == "false", "No",
     NA
    )
  ),
  levels = c("Yes", "No")
)


unique(updrs[["id38_3c.Nimmt.der.Patient.Levodopa.ein."]])
updrs <- updrs %>%
  rename(intake_levodopa = id38_3c.Nimmt.der.Patient.Levodopa.ein.)
updrs$intake_levodopa <- factor(
  ifelse(
    is.na(updrs$intake_levodopa), NA,
    ifelse(
      updrs$intake_levodopa == "true", "Yes",
      ifelse(
        updrs$intake_levodopa == "false", "No",
        NA 
      )
    )
  ),
  levels = c("Yes", "No")
)

unique(updrs[["id89_A.Traten.Dyskinesien..Chorea.oder.Dystonie..während.der.Untersuchung.auf."]])
updrs <- updrs %>%
  rename(dyskinesia_during_examination = id89_A.Traten.Dyskinesien..Chorea.oder.Dystonie..während.der.Untersuchung.auf.)
updrs$dyskinesia_during_examination <- factor(
  ifelse(
    is.na(updrs$dyskinesia_during_examination), NA,
    ifelse(
      updrs$dyskinesia_during_examination == "true", "Yes",
      ifelse(
        updrs$dyskinesia_during_examination == "false", "No",
        NA  
      )
    )
  ),
  levels = c("Yes", "No")
)

unique(updrs[["id37_3b.Falls.der.Patient.Medikamente.zur.Behandlung.der.Symptome.der.Parkinson.Krankheit.bekommt..geben.Sie.bitte.den.klinischen.Status.des.Patienten.unter.Verwendung.folgender.Begriffe.an.."]])
updrs <- updrs %>%
  rename(clinical_status_under_medication = id37_3b.Falls.der.Patient.Medikamente.zur.Behandlung.der.Symptome.der.Parkinson.Krankheit.bekommt..geben.Sie.bitte.den.klinischen.Status.des.Patienten.unter.Verwendung.folgender.Begriffe.an..)
updrs$clinical_status_under_medication <- factor(
  ifelse(
    is.na(updrs$clinical_status_under_medication),
    NA,
    ifelse(
      updrs$clinical_status_under_medication == "ON - ON ist der typische funktionelle Status, wenn die Patienten Medikamente bekommen und gut auf sie ansprechen. ",
      "ON",
      "OFF"
    )
  ),
  levels = c("ON", "OFF")
)
updrs_vars <- c(
  "primary_information_source",
  "intake_of_medication",
  "intake_levodopa",
  "dyskinesia_during_examination",
  "clinical_status_under_medication",
  "summaryindex_updrs"  
)

bdi_sel <- bdi %>% select(patient, group, Timepoint, bdi_score)
moca_sel <- moca %>% select(patient, group, Timepoint, all_of(moca_vars))
updrs_sel <- updrs %>% select(patient, group, Timepoint, all_of(updrs_vars))
hoehnyahr_sel <- hoehnyahr %>% select(patient, group, Timepoint, hoehnyahrlevel)
master_data <- bdi_sel %>%
  full_join(moca_sel, by = c("patient", "group", "Timepoint")) %>%
  full_join(updrs_sel, by = c("patient", "group", "Timepoint")) %>%
  full_join(hoehnyahr_sel, by = c("patient", "group", "Timepoint"))
master_data <- master_data %>%
  mutate(group_time = paste(group, Timepoint, sep = "_")) %>%
  mutate(group_time = factor(group_time,
                             levels = c("Control_T0", "Control_T6", "Intervention_T0", "Intervention_T6")))

vars_all <- c(bdi_vars, moca_vars, hoehnyahr_vars, updrs_vars)
tab_questionnaire <- CreateTableOne(
  vars = vars_all,
  strata = "group_time",
  data = master_data,
  includeNA = TRUE
)
print(tab_questionnaire, showAllLevels = TRUE, missing = TRUE)

tab_questionnaire_df <- print(
  tab_questionnaire,
  showAllLevels = TRUE,
  missing = TRUE,
  printToggle = FALSE,
  formatOptions = list(big.mark = ".", decimal.mark = ",")
)

tab_questionnaire_df <- tibble::rownames_to_column(as.data.frame(tab_questionnaire_df, stringsAsFactors = FALSE), "Variable")

write.csv(tab_questionnaire_df, "C:/Users/akoel/OneDrive/Desktop/Dr. Arbeit/descriptives.R/questionnaire_tableone.csv", row.names = FALSE)
