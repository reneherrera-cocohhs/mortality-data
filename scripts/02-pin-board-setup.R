# run this script once, after project setup it isn't necessary to run it again
# This script sets up the pin board; The pins package helps you publish data sets, models, and other R objects, making it easy to share them across projects and with your colleagues.
# rené dario herrera
# 12 Jan 2023
# rherrera at coconino dot az dot gov

# Setup ####
# load packages
pacman::p_load(
  here, # project oriented workflow
  pins # data access
)

# Pins ####
# create a pin board ####
mortality_folder <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/mortality-data")

# list the pins located on the pin board ####
mortality_folder %>%
  pin_list()

# use pin_meta to view the metadata of each pin ####
mortality_folder %>%
  pin_meta("mortality-data-2010-2021")

mortality_folder %>%
  pin_meta("mortality-data-ytd-2021-2022")
