# query.R
# Handles querying and filtering data within the DB connection

# Function to perform data filtering based on specific criteria
perform_data_filtering <- function(con, table_name) {
  tbl(con, "psid") |>
    filter(
      if_all(everything(), \(x) !is.na(x)),
      rel %in% c(1, 2),
      between(age, 18, 65),
      edu4 > 0,
      mlstat5 == 1,
      livpart == 1,
      emplst6 == 1,
      incjob1_mg > 0,
      hhinc_post > 0
    )
}

# Function to group and mutate data as per specific logic
group_and_mutate_data <- function(data_tbl) {
  data_tbl |>
    group_by(cpf_hid, wavey) |>
    filter(n() == 2, sum(female) == 1) |>
    mutate(
      part_income = sum(incjob1_mg) - incjob1_mg,
      female_income_share = if_else(
        female == 1, incjob1_mg / sum(incjob1_mg), part_income / sum(incjob1_mg)
      ),
      wife_earns_more = if_else(female_income_share >= 0.5, 1, 0),
      part_age = sum(age) - age,
      part_edu = sum(edu4) - edu4,
      mixed_couple = if_else(sum(hisp) == 1, 1, 0),
      kids = if_else(kidsn_hh17 > 0, 1, 0)
    ) |>
    ungroup()
}

# Example usage in combination:
# filtered_data <- perform_data_filtering(con, "psid")
# processed_data <- group_and_mutate_data(filtered_data)
