# Load necessary packages
library(readr)
library(dplyr)

# Define column positions and types
column_positions <- fwf_positions(
  start = c(1, 2, 6, 9, 10, 12, 16, 20, 23, 25, 27, 31, 32, 34, 38, 40, 44, 48, 50, 51),
  end = c(1, 5, 8, 9, 11, 15, 19, 22, 24, 26, 30, 31, 33, 37, 39, 43, 47, 49, 50, 52),
  col_names = c(
    "MH1", "MH2", "MH3", "MH4", "MH5", "MH6", "MH7", "MH8", "MH9", "MH10",
    "MH11", "MH12", "MH13", "MH14", "MH15", "MH16", "MH17", "MH18", "MH19", "MH20"
  )
)

# Read the file
df <- read_fwf("data/raw/MH85_21.txt", column_positions)

# Rename variables to add labels
df <- df |>
  rename(
    `RELEASE NUMBER` = MH1,
    `1968 INTERVIEW NUMBER OF INDIVIDUAL` = MH2,
    `PERSON NUMBER OF INDIVIDUAL` = MH3,
    `SEX OF INDIVIDUAL` = MH4,
    `MONTH INDIVIDUAL BORN` = MH5,
    `YEAR INDIVIDUAL BORN` = MH6,
    `1968 INTERVIEW NUMBER OF SPOUSE` = MH7,
    `PERSON NUMBER OF SPOUSE` = MH8,
    `ORDER OF THIS MARRIAGE` = MH9,
    `MONTH MARRIED` = MH10,
    `YEAR MARRIED` = MH11,
    `STATUS OF THIS MARRIAGE` = MH12,
    `MONTH WIDOWED OR DIVORCED` = MH13,
    `YEAR WIDOWED OR DIVORCED` = MH14,
    `MONTH SEPARATED` = MH15,
    `YEAR SEPARATED` = MH16,
    `YEAR MOST RECENTLY REPORTED MARRIAGE` = MH17,
    `NUMBER OF MARRIAGES OF THIS INDIVIDUAL` = MH18,
    `LAST KNOWN MARITAL STATUS` = MH19,
    `NUMBER OF MARRIAGE RECORDS` = MH20
  )
