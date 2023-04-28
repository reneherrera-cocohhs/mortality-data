# checks the "S:/HIPAA Compliance/SAS Files/Coconino Deaths/All Death/" folder for sas data files,
# loads them into the R data environment,
# transforms each variable to character,
# combines them into two data sets: 1-finalized and 2-year-to-date,
# saves combined data sets to "S:/HIPAA Compliance/SAS Files/Coconino Deaths/mortality-data" pin board folder
# 1- finalized by ADHS
# 2- year to date data not yet finalized

# rené dario herrera
# rherrera at coconino dot az dot gov
# coconino county az
# 10 January 2022

# Setup ####
# load packages
library(here) # project oriented workflow
library(tidyverse) # data reading, wrangling, and tidying
library(lubridate) # dates
library(haven) # SAS; import
library(janitor) # clean
library(pins) # data access
library(visdat)

# load pinboard; this is where we will save the transformed RDS files
mortality_folder <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/mortality-data")

# read finalized mortality data ####
# collect file names for function
filenames <- list.files(
  path = "S:/HIPAA Compliance/SAS Files/Coconino Deaths/All Death/",
  pattern = "coconino",
  full.names = TRUE
)

# collect file names to name environment objects
filenames_short <- list.files(
  path = "S:/HIPAA Compliance/SAS Files/Coconino Deaths/All Death/",
  pattern = "coconino",
  full.names = FALSE
)

# write function
func_read <- function(x, y) { # x = filename for sas; y = object name
  library(dplyr)
  tmp <- read_sas(x) %>%
    clean_names() %>%
    mutate(across(.cols = everything(), as.character)) %>%
    mutate(filename = as.character(x)) # create new variable of file name
  assign(y, tmp, envir = .GlobalEnv) # create data objects
}

# iteratively call function for each desired file
map2(
  .x = filenames,
  .y = filenames_short,
  .f = ~ func_read(.x, .y)
)

# inspect; which data objects were created?
ls(pattern = "coconino")

# combine to one data set
mortality_df <- mget(ls(pattern = "_deaths_")) %>%
  bind_rows()

# inspect
glimpse(mortality_df)

# dates, determine min and max
mortality_df_dates <- mortality_df %>%
  drop_na(death_book_year) %>%
  summarise(
    min = min(as.double(death_book_year)),
    max = max(as.double(death_book_year))
  )

# create character text string for pin board
mortality_df_name <- str_c(
  "mortality-data-",
  as.character(mortality_df_dates[, 1]),
  "-",
  as.character(mortality_df_dates[, 2])
)

# create character text string for pin board
mortality_df_title <- str_c(
  "AZDHS mortality extract, finalized (",
  as.character(mortality_df_dates[, 1]),
  "-",
  as.character(mortality_df_dates[, 2]),
  ")"
)

# save to pin board
mortality_folder %>%
  pin_write(
    x = mortality_df,
    name = mortality_df_name,
    title = mortality_df_title,
    type = "rds",
    description = str_c(
      "Finalized all cause mortality data extract provided by AZDHS. Includes data for Coconino County residents or deaths occurring in Coconino County. (",
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

# read year to date mortality data
# collect file names for function
filenames <- list.files(
  path = "S:/HIPAA Compliance/SAS Files/Coconino Deaths/All Death/",
  pattern = "ytddeaths",
  full.names = TRUE
)

# collect file names to name environment objects
filenames_short <- list.files(
  path = "S:/HIPAA Compliance/SAS Files/Coconino Deaths/All Death/",
  pattern = "ytddeaths",
  full.names = FALSE
)

# write function
func_read <- function(x, y) { # x = filename for sas; y = object name
  tmp <- read_sas(x) %>%
    clean_names() %>%
    # mutate(across(.cols = everything(), as.character)) %>%
    mutate(filename = as.character(x)) # create new variable of file name
  assign(y, tmp, envir = .GlobalEnv) # create data objects
}

# iteratively call function for each desired file
map2(
  .x = filenames,
  .y = filenames_short,
  .f = ~ func_read(.x, .y)
)

# inspect; which data objects exist?
ls(pattern = "ytddeaths")

# combine to one data set
mortality_df_ytd <- mget(ls(pattern = "ytddeaths")) %>%
  bind_rows()

# inspect
glimpse(mortality_df_ytd)

# count
mortality_df_ytd %>%
  tabyl(death_book_year)

# dates; min and max dates of data set
mortality_df_ytd_dates <- mortality_df_ytd %>%
  drop_na(death_book_year) %>%
  summarise(
    min = min(as.double(death_book_year)),
    max = max(as.double(death_book_year))
  )

# create character text string for pin board
mortality_df_ytd_name <- str_c(
  "mortality-data-ytd-",
  as.character(mortality_df_ytd_dates[, 1]),
  "-",
  as.character(mortality_df_ytd_dates[, 2])
)

# create character text string for pin board
mortality_df_ytd_title <- str_c(
  "AZDHS mortality extract, year to date (",
  as.character(mortality_df_ytd_dates[, 1]),
  "-",
  as.character(mortality_df_ytd_dates[, 2]),
  ")"
)

# inspect
mortality_df_ytd %>%
  tabyl(death_book_year)

mortality_df_ytd %>%
  tabyl(death_book_year, decedent_gender_desc)

mortality_df_ytd %>%
  ggplot() +
  geom_bar(
    mapping = aes(
      x = death_book_year,
      fill = decedent_gender_desc
    ),
    position = "dodge"
  )

mortality_df_ytd %>%
  group_by(death_book_year) %>%
  ggplot() +
  geom_boxplot(
    mapping = aes(
      y = as.double(decedent_years),
      x = death_book_year,
      fill = as.factor(decedent_gender_desc)
    )
  )

# save to pin board
# YTD
mortality_folder %>%
  pin_write(
    x = mortality_df_ytd,
    name = mortality_df_ytd_name,
    title = mortality_df_ytd_title,
    type = "rds",
    description = str_c(
      "Year-to-date all cause mortality data extract provided by AZDHS. Includes data for Coconino County residents or deaths occurring in Coconino County. (",
      as.character(mortality_df_ytd_dates[, 1]),
      "-",
      as.character(mortality_df_ytd_dates[, 2]),
      ")"
    ),
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )
