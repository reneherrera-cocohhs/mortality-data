# create new variables to describe the data
# create subsets of the data
# key ####
# create variables that starts with:
# d_ ; dummy variables
# calc_ ; calculated


# rené dario herrera
# 12 Jan 2023
# rherrera at coconino dot az dot gov

# Setup ####
# load package libraries
library(here) # project oriented workflow
library(pins) # data access
library(tidyverse)
library(janitor)
library(tidytext)
library(stopwords)

# custom functions
# function to code words
# tokenize words in character variable, remove stop words,
# then produce a frequency table of words
code_words <- function(.data, x) {
  library(dplyr)
  library(tidytext)
  library(stopwords)
  .data %>%
    mutate(
      text = {{ x }},
      line = row_number()
    ) %>%
    unnest_tokens(
      output = word,
      input = text,
      token = "words"
    ) %>%
    anti_join(get_stopwords()) %>%
    tabyl(word) %>%
    as_tibble() %>%
    arrange(desc(n)) %>%
    print(n = 25)
}

# Pins ####
# create a pin board
mortality_folder <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/mortality-data")

# list the pins located on the pin board
mortality_folder %>%
  pin_list()

# check pin meta data 
mortality_folder %>%
  pin_meta("mortality-data-combined-2010-2023")

# read historical finalized mortality data to the environment
mortality_data <- mortality_folder %>%
  pin_read("mortality-data-combined-2010-2023")

# inspect
glimpse(mortality_data)

# age and age groups  ####
# create levels for age group coding as factor below
age_group_5yr_levels <- c(
  "0-4 years",
  "5-9 years",
  "10-14 years",
  "15-19 years",
  "20-24 years",
  "25-29 years",
  "30-34 years",
  "35-39 years",
  "40-44 years",
  "45-49 years",
  "50-54 years",
  "55-59 years",
  "60-64 years",
  "65-69 years",
  "70-74 years",
  "75-79 years",
  "80-84 years",
  "85+ years"
)

age_group_7cat_levels <- c(
  "Under 5 years",
  "5 to 14 years",
  "15 to 19 years",
  "20 to 44 years",
  "45 to 64 years",
  "65 to 69 years",
  "70 years and over"
)

age_group_6cat_levels <- c(
  "Under 14 years",
  "14 to 17 years",
  "18 to 25 years",
  "26 to 44 years",
  "45 to 64 years",
  "65 years and over"
)

mortality_data <- mortality_data %>%
  mutate(
    d_age_group_5yr = factor(case_when( # for age adjustment
      calc_age < 5 ~ "0-4 years",
      calc_age < 10 ~ "5-9 years",
      calc_age < 15 ~ "10-14 years",
      calc_age < 20 ~ "15-19 years",
      calc_age < 25 ~ "20-24 years",
      calc_age < 30 ~ "25-29 years",
      calc_age < 35 ~ "30-34 years",
      calc_age < 40 ~ "35-39 years",
      calc_age < 45 ~ "40-44 years",
      calc_age < 50 ~ "45-49 years",
      calc_age < 55 ~ "50-54 years",
      calc_age < 60 ~ "55-59 years",
      calc_age < 65 ~ "60-64 years",
      calc_age < 70 ~ "65-69 years",
      calc_age < 75 ~ "70-74 years",
      calc_age < 80 ~ "75-79 years",
      calc_age < 85 ~ "80-84 years",
      calc_age >= 85 ~ "85+ years",
      TRUE ~ as.character(calc_age)
    ), levels = age_group_5yr_levels, ordered = TRUE), #
    d_age_group_7cat = factor(case_when( # for reporting
      calc_age < 5 ~ "Under 5 years",
      calc_age < 15 ~ "5 to 14 years",
      calc_age < 20 ~ "15 to 19 years",
      calc_age < 45 ~ "20 to 44 years",
      calc_age < 65 ~ "45 to 64 years",
      calc_age < 70 ~ "65 to 69 years",
      calc_age >= 70 ~ "70 years and over",
      TRUE ~ as.character(calc_age)
    ), levels = age_group_7cat_levels, ordered = TRUE),
    d_age_group_6cat = factor(case_when( # for reporting
      calc_age < 14 ~ "Under 14 years",
      calc_age < 18 ~ "14 to 17 years",
      calc_age < 26 ~ "18 to 25 years",
      calc_age < 45 ~ "26 to 44 years",
      calc_age < 65 ~ "45 to 64 years",
      calc_age >= 65 ~ "65 years and over",
      TRUE ~ as.character(calc_age)
    ), levels = age_group_6cat_levels, ordered = TRUE)
  )

# frequency tables for age groups
mortality_data %>%
  tabyl(d_age_group_5yr)

mortality_data %>%
  ggplot() +
  geom_bar(
    mapping = aes(
      x = d_age_group_5yr
    )
  )

mortality_data %>%
  tabyl(d_age_group_7cat)

mortality_data %>%
  ggplot() +
  geom_bar(
    mapping = aes(
      x = d_age_group_7cat
    )
  )

mortality_data %>%
  tabyl(d_age_group_6cat)

mortality_data %>%
  ggplot() +
  geom_bar(
    mapping = aes(
      x = d_age_group_6cat
    )
  )

# race and ethnicity groups ####
mortality_data <- mortality_data %>%
  mutate(
    d_race_code = factor(
      case_when(
        str_to_lower(race_vsims) == "hispanic or latino" ~ "Hispanic or Latino (any race)",
        str_to_lower(race_vsims) == "white non-hispanic" ~ "White Non-Hispanic",
        str_to_lower(race_vsims) == "black or african american" ~ "Black or African American",
        str_to_lower(race_vsims) == "american indian or alaska native" ~ "American Indian and Alaska Native",
        str_to_lower(race_vsims) == "asian or pacific islander" ~ "Asian or Native Hawaiian and Other Pacific Islander",
        str_to_lower(race_vsims) == "unknown/other/refused" ~ "Other",
        is.na(race_vsims) ~ "Other",
        TRUE ~ "Other"
      ),
      exclude = NA,
      levels = c(
        "White Non-Hispanic",
        "American Indian and Alaska Native",
        "Hispanic or Latino (any race)",
        "Black or African American",
        "Asian or Native Hawaiian and Other Pacific Islander",
        "Other"
      )
    )
  )

# frequency tables
mortality_data %>%
  tabyl(d_race_code)

mortality_data %>%
  ggplot() +
  geom_bar(
    mapping = aes(
      x = d_race_code
    )
  )

# sex and gender ####
mortality_data <- mortality_data %>%
  mutate(
    d_sex = factor(
      case_when(
        str_to_lower(decedent_gender_desc) == "male" ~ "Male",
        str_to_lower(decedent_gender_desc) == "female" ~ "Female",
        is.na(decedent_gender_desc) ~ "Not yet determined",
        TRUE ~ "Not yet determined"
      ),
      levels = c("Female", "Male", "Not yet determined")
    )
  )

# frequency tables
mortality_data %>%
  tabyl(d_sex)

mortality_data %>%
  ggplot() +
  geom_bar(
    mapping = aes(
      x = d_sex
    )
  )

# residence in coconino county ####
mortality_data <- mortality_data %>%
  mutate(
    d_county_resident = if_else(
      str_detect(str_to_lower(residence_county_name), "coconino"),
      "Resident",
      "Non-Resident"
    ),
    d_county_resident = replace_na(d_county_resident, "Non-Resident")
  )


# frequency tables
mortality_data %>%
  tabyl(d_county_resident)

mortality_data %>%
  ggplot() +
  geom_bar(
    mapping = aes(
      x = d_county_resident
    )
  )

# Zip codes ####
mortality_data <- mortality_data %>%
  mutate(
    d_decedent_zip = residence_zip,
    d_decedent_zip = case_when(
      is.na(d_decedent_zip) ~ n_zip,
      TRUE ~ d_decedent_zip
    ),
    d_decedent_zip = case_when(
      is.na(d_decedent_zip) ~ zip_code,
      TRUE ~ d_decedent_zip
    ),
    d_decedent_zip = case_when(
      is.na(d_decedent_zip) ~ "Unknown",
      TRUE ~ d_decedent_zip
    )
  )

# frequency tables
mortality_data %>%
  mutate(
    d_decedent_zip = fct_lump_n(
      f = as.factor(d_decedent_zip),
      n = 10
    )
  ) %>%
  tabyl(d_decedent_zip)

mortality_data %>%
  mutate(
    d_decedent_zip = fct_lump_n(
      f = as.factor(d_decedent_zip),
      n = 10
    )
  ) %>%
  ggplot() +
  geom_bar(
    mapping = aes(
      x = d_decedent_zip
    )
  )

# tribal ####

# I'm not sure how to go about this

# highest educational attainment ####
mortality_data <- mortality_data %>%
  mutate(
    d_edu_code = factor(
      case_when(
        str_detect(str_to_lower(education_desc), "8th grade or less") ~ "8th grade or less",
        str_detect(str_to_lower(education_desc), "9th") ~ "9th-12th grade; no diploma",
        str_detect(str_to_lower(education_desc), "associate degree") ~ "Associate degree",
        str_detect(str_to_lower(education_desc), "bachelor's degree") ~ "Bachelor's degree",
        str_detect(str_to_lower(education_desc), "doctorate") ~ "Doctorate or professional degree",
        str_detect(str_to_lower(education_desc), "high school graduate") ~ "High school graduate or GED",
        str_detect(str_to_lower(education_desc), "master's degree") ~ "Master's degree",
        str_detect(str_to_lower(education_desc), "not classifiable") ~ "Unknown",
        str_detect(str_to_lower(education_desc), "some college credit") ~ "Some college, but no degree",
        str_detect(str_to_lower(education_desc), "unknown") ~ "Unknown",
        is.na(education_desc) ~ "Unknown",
        TRUE ~ "Unknown"
      )
    ),
    d_hs_grad = factor(
      case_when(
        str_detect(str_to_lower(education_desc), "8th grade or less") ~ "No",
        str_detect(str_to_lower(education_desc), "9th") ~ "No",
        str_detect(str_to_lower(education_desc), "associate degree") ~ "Yes",
        str_detect(str_to_lower(education_desc), "bachelor's degree") ~ "Yes",
        str_detect(str_to_lower(education_desc), "doctorate") ~ "Yes",
        str_detect(str_to_lower(education_desc), "high school graduate") ~ "Yes",
        str_detect(str_to_lower(education_desc), "master's degree") ~ "Yes",
        str_detect(str_to_lower(education_desc), "not classifiable") ~ "Unknown",
        str_detect(str_to_lower(education_desc), "some college credit") ~ "Yes",
        str_detect(str_to_lower(education_desc), "unknown") ~ "Unknown",
        is.na(education_desc) ~ "Unknown",
        TRUE ~ "Unknown"
      )
    ),
    d_college_grad = factor(
      case_when(
        str_detect(str_to_lower(education_desc), "8th grade or less") ~ "No",
        str_detect(str_to_lower(education_desc), "9th") ~ "No",
        str_detect(str_to_lower(education_desc), "associate degree") ~ "No",
        str_detect(str_to_lower(education_desc), "bachelor's degree") ~ "Yes",
        str_detect(str_to_lower(education_desc), "doctorate") ~ "Yes",
        str_detect(str_to_lower(education_desc), "high school graduate") ~ "No",
        str_detect(str_to_lower(education_desc), "master's degree") ~ "Yes",
        str_detect(str_to_lower(education_desc), "not classifiable") ~ "Unknown",
        str_detect(str_to_lower(education_desc), "some college credit") ~ "No",
        str_detect(str_to_lower(education_desc), "unknown") ~ "Unknown",
        is.na(education_desc) ~ "Unknown",
        TRUE ~ "Unknown"
      )
    )
  )

# frequency tables
mortality_data %>%
  tabyl(d_edu_code)

mortality_data %>%
  ggplot() +
  geom_bar(
    mapping = aes(
      x = d_edu_code
    )
  )

# frequency tables
mortality_data %>%
  tabyl(d_hs_grad)

mortality_data %>%
  ggplot() +
  geom_bar(
    mapping = aes(
      x = d_hs_grad
    )
  )

# frequency tables
mortality_data %>%
  tabyl(d_college_grad)

mortality_data %>%
  ggplot() +
  geom_bar(
    mapping = aes(
      x = d_college_grad
    )
  )

# marital status ####
mortality_data <- mortality_data %>%
  mutate(
    d_marital_code = factor(
      case_when(
        str_detect(str_to_lower(marital_desc), "divorced") ~ "Divorced",
        str_detect(str_to_lower(marital_desc), "married but") ~ "Married, but separated",
        str_detect(str_to_lower(marital_desc), "never married") ~ "Never married, single",
        str_detect(str_to_lower(marital_desc), "married") ~ "Married",
        str_detect(str_to_lower(marital_desc), "widowed") ~ "Widowed",
        str_detect(str_to_lower(marital_desc), "unknown") ~ "Unknown",
        is.na(marital_desc) ~ "Unknown",
        TRUE ~ "Unknown"
      )
    )
  )

# frequency tables
mortality_data %>%
  tabyl(d_marital_code)

mortality_data %>%
  ggplot() +
  geom_bar(
    mapping = aes(
      x = d_marital_code
    )
  )

# occupation ####
mortality_data %>%
  code_words(x = occupation_description)

mortality_data <- mortality_data %>%
  mutate(
    d_occupation_code = factor(
      str_to_lower(case_when(
        str_detect(str_to_lower(occupation_description), "home maker") ~ "homemaker",
        str_detect(str_to_lower(occupation_description), "student") ~ "student",
        str_detect(str_to_lower(occupation_description), "lawyer") ~ "attorney",
        str_detect(str_to_lower(occupation_description), "police officer") ~ "law enforcement",
        str_detect(str_to_lower(occupation_description), "teacher") ~ "teacher",
        str_detect(str_to_lower(occupation_description), "engineer") ~ "engineer",
        str_detect(str_to_lower(occupation_description), "carpenter") ~ "carpenter",
        str_detect(str_to_lower(occupation_description), "laborer") ~ "laborer",
        str_detect(str_to_lower(occupation_description), "driver") ~ "driver",
        str_detect(str_to_lower(occupation_description), "nurse") ~ "nurse",
        str_detect(str_to_lower(occupation_description), "manager") ~ "manager",
        str_detect(str_to_lower(occupation_description), "mechanic") ~ "mechanic",
        is.na(occupation_description) ~ "unknown",
        TRUE ~ as.character(occupation_description)
      ))
    )
  )

# frequency tables
mortality_data %>%
  mutate(
    d_occupation_code = fct_lump_n(
      f = as.factor(d_occupation_code),
      n = 10
    )
  ) %>%
  tabyl(d_occupation_code)

mortality_data %>%
  mutate(
    d_occupation_code = fct_lump_n(
      f = as.factor(d_occupation_code),
      n = 10
    )
  ) %>%
  ggplot() +
  geom_bar(
    mapping = aes(
      x = d_occupation_code
    )
  )

# industry ####
mortality_data %>%
  code_words(x = industry_description)

mortality_data <- mortality_data %>%
  mutate(
    d_industry_code = factor(
      str_to_lower(case_when(
        str_detect(str_to_lower(industry_description), "home") ~ "home",
        str_detect(str_to_lower(industry_description), "construction") ~ "construction",
        str_detect(str_to_lower(industry_description), "education") ~ "education",
        str_detect(str_to_lower(industry_description), "government") ~ "government",
        str_detect(str_to_lower(industry_description), "retail") ~ "retail",
        str_detect(str_to_lower(industry_description), "automotive") ~ "automotive",
        str_detect(str_to_lower(industry_description), "restaurant") ~ "restaurant",
        str_detect(str_to_lower(industry_description), "school") ~ "education",
        str_detect(str_to_lower(industry_description), "university") ~ "education",
        str_detect(str_to_lower(industry_description), "railroad") ~ "railroad",
        str_detect(str_to_lower(industry_description), "sales") ~ "sales",
        str_detect(str_to_lower(industry_description), "transportation") ~ "transportation",
        str_detect(str_to_lower(industry_description), "health") ~ "healthcare",
        str_detect(str_to_lower(industry_description), "manufacturing") ~ "manufacturing",
        str_detect(str_to_lower(industry_description), "medical") ~ "healthcare",
        is.na(industry_description) ~ "unknown",
        TRUE ~ as.character(industry_description)
      ))
    )
  )

# frequency tables
mortality_data %>%
  mutate(
    d_industry_code = fct_lump_n(
      f = as.factor(d_industry_code),
      n = 10
    )
  ) %>%
  tabyl(d_industry_code)

mortality_data %>%
  mutate(
    d_industry_code = fct_lump_n(
      f = as.factor(d_industry_code),
      n = 10
    )
  ) %>%
  ggplot() +
  geom_bar(
    mapping = aes(
      x = d_industry_code
    )
  )

# injury description ####
mortality_data %>%
  code_words(x = cdc_injurydesc)

mortality_data <- mortality_data %>%
  mutate(
    d_injurydesc_code = factor(
      str_to_lower(case_when(
        str_detect(str_to_lower(cdc_injurydesc), "overdose") ~ "overdose/poison",
        str_detect(str_to_lower(cdc_injurydesc), "poison") ~ "overdose/poison",
        str_detect(str_to_lower(cdc_injurydesc), "ingested") ~ "overdose/poison",
        str_detect(str_to_lower(cdc_injurydesc), "substance") ~ "overdose/poison",
        str_detect(str_to_lower(cdc_injurydesc), "methamphetamine") ~ "overdose/poison",
        str_detect(str_to_lower(cdc_injurydesc), "fentanyl") ~ "overdose/poison",
        str_detect(str_to_lower(cdc_injurydesc), "heroin") ~ "overdose/poison",
        str_detect(str_to_lower(cdc_injurydesc), "intoxication") ~ "intoxication",
        str_detect(str_to_lower(cdc_injurydesc), "alcohol") ~ "intoxication",
        str_detect(str_to_lower(cdc_injurydesc), "ethanol") ~ "intoxication",
        str_detect(str_to_lower(cdc_injurydesc), "firearm") ~ "firearm",
        str_detect(str_to_lower(cdc_injurydesc), "shot") ~ "firearm",
        str_detect(str_to_lower(cdc_injurydesc), "gun") ~ "firearm",
        str_detect(str_to_lower(cdc_injurydesc), "driver") ~ "motor vehicle",
        str_detect(str_to_lower(cdc_injurydesc), "passenger") ~ "motor vehicle",
        str_detect(str_to_lower(cdc_injurydesc), "occupant") ~ "motor vehicle",
        str_detect(str_to_lower(cdc_injurydesc), "motor vehicle") ~ "motor vehicle",
        str_detect(str_to_lower(cdc_injurydesc), "ejected") ~ "motor vehicle",
        str_detect(str_to_lower(cdc_injurydesc), "struck by") ~ "motor vehicle",
        str_detect(str_to_lower(cdc_injurydesc), "train") ~ "train",
        str_detect(str_to_lower(cdc_injurydesc), "fall") ~ "jump/fall",
        str_detect(str_to_lower(cdc_injurydesc), "fell") ~ "jump/fall",
        str_detect(str_to_lower(cdc_injurydesc), "hang") ~ "hanging/asphyxiation",
        str_detect(str_to_lower(cdc_injurydesc), "fire") ~ "fire/smoke inhalation",
        str_detect(str_to_lower(cdc_injurydesc), "smoke") ~ "fire/smoke inhalation",
        TRUE ~ as.character(cdc_injurydesc)
      ))
    )
  )

# frequency tables
mortality_data %>%
  mutate(
    d_injurydesc_code = fct_lump_n(
      f = as.factor(d_injurydesc_code),
      n = 10
    )
  ) %>%
  tabyl(d_injurydesc_code)

mortality_data %>%
  mutate(
    d_injurydesc_code = fct_lump_n(
      f = as.factor(d_injurydesc_code),
      n = 10
    )
  ) %>%
  ggplot() +
  geom_bar(
    mapping = aes(
      x = d_injurydesc_code
    )
  )

# manner of death ####
mortality_data %>%
  code_words(x = cod_a)

mortality_data <- mortality_data %>%
  mutate(
    d_cod_a_code = factor(
      str_to_lower(case_when(
        str_detect(str_to_lower(cod_a), "cancer") ~ "cancer",
        str_detect(str_to_lower(cod_a), "cardiopulmonary") ~ "cardiopulmonary arrest",
        str_detect(str_to_lower(cod_a), "cardiac") ~ "heart disease",
        str_detect(str_to_lower(cod_a), "respiratory failure") ~ "respiratory failure/arrest",
        str_detect(str_to_lower(cod_a), "respiratory arrest") ~ "respiratory failure/arrest",
        str_detect(str_to_lower(cod_a), "heart failure") ~ "heart disease",
        str_detect(str_to_lower(cod_a), "cardiovascular disease") ~ "heart disease",
        str_detect(str_to_lower(cod_a), "gunshot") ~ "firearm/gunshot",
        str_detect(str_to_lower(cod_a), "pending") ~ "unknown",
        str_detect(str_to_lower(cod_a), "covid") ~ "COVID-19",
        TRUE ~ as.character(cod_a)
      ))
    )
  )

# frequency tables
mortality_data %>%
  mutate(
    d_cod_a_code = fct_lump_n(
      f = as.factor(d_cod_a_code),
      n = 10
    )
  ) %>%
  tabyl(d_cod_a_code)

mortality_data %>%
  mutate(
    d_cod_a_code = fct_lump_n(
      f = as.factor(d_cod_a_code),
      n = 10
    )
  ) %>%
  ggplot() +
  geom_bar(
    mapping = aes(
      x = d_cod_a_code
    )
  )


# Suicide ####
# create dummy variable if suicide
mortality_data %>%
  code_words(x = cdc_mannerofdeath_desc)

mortality_data <- mortality_data %>%
  mutate(
    d_suicide = if_else(
      condition = str_to_lower(cdc_mannerofdeath_desc) == "suicide",
      true = 1,
      false = 0
    ),
    d_homicide = if_else(
      condition = str_to_lower(cdc_mannerofdeath_desc) == "homicide",
      true = 1,
      false = 0
    ),
    d_accident = if_else(
      condition = str_to_lower(cdc_mannerofdeath_desc) == "accident",
      true = 1,
      false = 0
    ),
    d_natural_death = if_else(
      condition = str_to_lower(cdc_mannerofdeath_desc) == "natural death",
      true = 1,
      false = 0
    )
  )

# frequency tables
mortality_data %>%
  pivot_longer(
    cols = c("d_suicide", "d_homicide", "d_accident", "d_natural_death"),
    names_to = "d_variable"
  ) %>%
  tabyl(d_variable, value)

mortality_data %>%
  pivot_longer(
    cols = c("d_suicide", "d_homicide", "d_accident", "d_natural_death"),
    names_to = "d_variable"
  ) %>%
  ggplot() +
  geom_col(
    mapping = aes(
      x = d_variable,
      y = value
    )
  )

# ICD10 codes ####
# applying ICD10 codes to create new dummy variables
source(
  file = "scripts/03-icd10-codes.R",
  echo = TRUE
)

# select variables where ICD10 is indicated
var_icd <- c(
  mortality_data %>%
    select(contains("icd")) %>%
    names(),
  mortality_data %>%
    select(contains("cod_")) %>%
    names(),
  mortality_data %>%
    select(contains("_cause")) %>%
    names(),
  mortality_data %>%
    select(contains("cdc")) %>%
    names()
)

# BROKEN? ####

# create new dummy variables based on ICD10 codes
mortality_data <- mortality_data %>%
  mutate(across(where(is.character), str_to_lower)) %>%
  mutate(
    d_alcohol = if_any(contains(var_icd), ~ str_detect(.x, (icd10_alcohol))),
    d_amphetamines = if_any(contains(var_icd), ~ str_detect(.x, (icd10_amphetamines))),
    d_cannabis = if_any(contains(var_icd), ~ str_detect(.x, (icd10_cannabis))),
    d_cocaine = if_any(contains(var_icd), ~ str_detect(.x, (icd10_cocaine))),
    d_hallucinagens = if_any(contains(var_icd), ~ str_detect(.x, (icd10_hallucinagens))),
    d_heroin = if_any(contains(var_icd), ~ str_detect(.x, (icd10_heroin))),
    d_mv = if_any(contains(var_icd), ~ str_detect(.x, (icd10_mv))),
    d_opioids = if_any(contains(var_icd), ~ str_detect(.x, (icd10_opioids))),
    d_opioids_rx = if_any(contains(var_icd), ~ str_detect(.x, (icd10_opioids_rx))),
    d_poly = if_any(contains(var_icd), ~ str_detect(.x, (icd10_poly))),
    d_non_opioids_rx = if_any(contains(var_icd), ~ str_detect(.x, (icd10_rx_non_opioids))),
    d_sedatives = if_any(contains(var_icd), ~ str_detect(.x, (icd10_sedatives))),
    d_selfinjury = if_any(contains(var_icd), ~ str_detect(.x, (icd10_selfinjury))),
    d_opioids_any = if_any(contains(var_icd), ~ str_detect(.x, paste(c(icd10_heroin, icd10_opioids, icd10_opioids_rx), collapse = "|")))
  )

# substance abuse ####
mortality_data <- mortality_data %>%
  mutate(
    d_substance_abuse = case_when(
      d_alcohol == TRUE ~ TRUE,
      d_amphetamines == TRUE ~ TRUE,
      d_cannabis == TRUE ~ TRUE,
      d_cocaine == TRUE ~ TRUE,
      d_hallucinagens == TRUE ~ TRUE,
      d_heroin == TRUE ~ TRUE,
      d_opioids == TRUE ~ TRUE,
      d_opioids_rx == TRUE ~ TRUE,
      d_poly == TRUE ~ TRUE,
      d_non_opioids_rx == TRUE ~ TRUE,
      d_sedatives == TRUE ~ TRUE,
      TRUE ~ FALSE
    )
  )

# change NA in dummy logical variables to FALSE ####
mortality_data <- mortality_data %>%
  mutate(
    across(
      .cols = (starts_with("d_") & where(is.logical)),
      ~ replace_na(.x, FALSE)
    )
  )

# inspect new dummy variables
mortality_data %>%
  slice_sample(n = 20) %>%
  select(starts_with("d_")) %>%
  glimpse()

# check dummy variables for each year
mortality_data %>%
  tabyl(
    death_book_year, d_suicide
  )

mortality_data %>%
  tabyl(
    death_book_year, d_homicide
  )

mortality_data %>%
  tabyl(
    death_book_year, d_opioids_any
  )

mortality_data %>%
  tabyl(
    death_book_year, d_opioids
  )

mortality_data %>%
  tabyl(
    death_book_year, d_opioids_rx
  )

mortality_data %>%
  tabyl(
    death_book_year, d_alcohol
  )

# save to pin board ####
# dates, determine min and max
mortality_df_dates <- mortality_data %>%
  drop_na(death_book_year) %>%
  summarise(
    min = min(as.double(death_book_year)),
    max = max(as.double(death_book_year))
  )

mortality_df_dates

# create character text string for pin board
mortality_df_name <- str_c(
  "mortality-data-tidy-transformed-",
  as.character(mortality_df_dates[, 1]),
  "-",
  as.character(mortality_df_dates[, 2])
)

mortality_df_name

# create character text string for pin board
mortality_df_title <- str_c(
  "AZDHS mortality extract, tidy, transformed, includes final and year-to-date (",
  as.character(mortality_df_dates[, 1]),
  "-",
  as.character(mortality_df_dates[, 2]),
  ")"
)

mortality_df_title

# save to pin board
mortality_folder %>%
  pin_write(
    x = mortality_data,
    name = mortality_df_name,
    title = mortality_df_title,
    type = "rds",
    description = str_c(
      "Finalized and year-to-date all cause mortality data extract provided by AZDHS. Includes data for Coconino County residents or deaths occurring in Coconino County. Tidy and transformed. (",
      as.character(mortality_df_dates[, 1]),
      "-",
      as.character(mortality_df_dates[, 2]),
      ")"
    ),
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

# check meta data
mortality_folder %>%
  pin_meta(mortality_df_name)
