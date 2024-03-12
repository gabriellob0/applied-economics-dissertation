#' Test for variation in 'hisp' within 'cpf_pid'
#'
#' This function checks if there is any variation in the 'hisp' variable within each 'cpf_pid' group.
#'
#' @param model_data A data frame containing the 'cpf_pid' and 'hisp' variables.
#'
#' @return TRUE if there is no variation in 'hisp' within 'cpf_pid', FALSE otherwise.
test_variation_in_hisp <- function(model_data) {
  rows <- model_data |>
    group_by(cpf_pid) |>
    summarise(variation_in_hisp = n_distinct(hisp)) |>
    filter(variation_in_hisp > 1) |>
    nrow()
  
  rows == 0
}

#' Count non-Hispanic households with two members per wave
#'
#' This function filters the dataset to include only non-Hispanic households and counts the number of households
#' with two members in each wave.
#'
#' @param model_data A data frame containing the 'cpf_hid', 'wavey', and 'hisp' variables.
#'
#' @return A data frame with the count of non-Hispanic households with two members per wave.
count_non_hispanic_households <- function(model_data) {
  model_data |>
    filter(hisp == 0) |>
    group_by(cpf_hid, wavey) |>
    filter(n() == 2) |>
    summarise(count = n()) |>
    ungroup()
}

#' Count Hispanic households per wave
#'
#' This function filters the dataset to include only Hispanic households and counts the number of distinct
#' households in each wave.
#'
#' @param model_data A data frame containing the 'cpf_hid', 'wavey', and 'hisp' variables.
#'
#' @return A data frame with the count of distinct Hispanic households per wave.
count_hispanic_households_per_wave <- function(model_data) {
  model_data |>
    filter(hisp == 1) |>
    group_by(wavey) |>
    summarise(count = n_distinct(cpf_hid))
}

#' Summarize housework by gender and Hispanic origin
#'
#' This function calculates the average housework hours, number of observations, distinct individuals, and
#' distinct households by gender and Hispanic origin.
#'
#' @param model_data A data frame containing the 'cpf_pid', 'cpf_hid', 'female', 'hisp', and 'hwork' variables.
#'
#' @return A data frame with the summary statistics for housework by gender and Hispanic origin.
summarize_housework_by_gender_hisp <- function(model_data) {
  model_data |>
    select(cpf_pid, cpf_hid, female, hisp, hwork) |>
    group_by(female, hisp) |>
    summarise(
      housework = mean(hwork, na.rm = TRUE),
      observations = n(),
      individuals = n_distinct(cpf_pid),
      households = n_distinct(cpf_hid)
    )
}

#' Count non-Hispanic individuals per household and wave
#'
#' This function filters the dataset to include only non-Hispanic individuals and counts the number of
#' distinct individuals per household and wave.
#'
#' @param model_data A data frame containing the 'cpf_pid', 'cpf_hid', 'wavey', and 'hisp' variables.
#'
#' @return A data frame with the count of distinct non-Hispanic individuals per household and wave.
count_non_hispanic_individuals_per_household_wave <- function(model_data) {
  model_data |>
    select(cpf_pid, cpf_hid, wavey, hisp) |>
    filter(hisp == 0) |>
    group_by(cpf_hid, wavey) |>
    summarise(count = n_distinct(cpf_pid), .groups = "drop")
}

#' Identify non-Hispanic individuals in multiple households
#'
#' This function filters the dataset to include only non-Hispanic individuals and identifies those who appear
#' in multiple households.
#'
#' @param model_data A data frame containing the 'cpf_pid', 'cpf_hid', and 'hisp' variables.
#'
#' @return A data frame with non-Hispanic individuals who appear in multiple households.
identify_non_hispanic_individuals_multiple_households <- function(model_data) {
  model_data |>
    select(cpf_pid, cpf_hid, hisp) |>
    filter(hisp == 0) |>
    group_by(cpf_pid) |>
    summarise(num_hid = n_distinct(cpf_hid), .groups = "drop") |>
    filter(num_hid > 1)
}

#' Identify non-Hispanic households with more than two individuals
#'
#' This function filters the dataset to include only non-Hispanic households and identifies those with more
#' than two distinct individuals.
#'
#' @param model_data A data frame containing the 'cpf_pid', 'cpf_hid', and 'hisp' variables.
#'
#' @return A data frame with non-Hispanic households that have more than two distinct individuals.
identify_non_hispanic_households_more_than_two <- function(model_data) {
  model_data |>
    select(cpf_pid, cpf_hid, hisp) |>
    filter(hisp == 0) |>
    group_by(cpf_hid) |>
    summarise(num_pid = n_distinct(cpf_pid), .groups = "drop") |>
    filter(num_pid > 2)
}

#' Identify same-sex couples
#'
#' This function identifies same-sex couples in the dataset based on the 'female' variable.
#'
#' @param model_data A data frame containing the 'cpf_pid', 'cpf_hid', 'wavey', 'hisp', and 'female' variables.
#'
#' @return A data frame with same-sex couples, or a message indicating that there are no same-sex couples in the dataset.
identify_same_sex_couples <- function(model_data) {
  same_sex_couples <- model_data |>
    select(cpf_pid, cpf_hid, wavey, hisp, female) |>
    filter(hisp == 0) |>
    group_by(cpf_hid, wavey) |>
    filter(n_distinct(female) == 1 & n() > 1) |>
    ungroup()
  
  if (nrow(same_sex_couples) > 0) {
    return(same_sex_couples)
  } else {
    return("There are no same-sex couples in the dataset.")
  }
}