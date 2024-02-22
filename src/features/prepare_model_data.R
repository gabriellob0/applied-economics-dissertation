# wrangling.R
# Prepares data for modeling

# Function to finalize data for modeling
prepare_model_data <- function(data_tbl) {
  data_tbl |>
    mutate(
      edu4 = as.factor(edu4),
      part_edu = as.factor(part_edu),
      rstate = as.factor(rstate)
    ) |>
    filter(mixed_couple == 0) |>
    select(-mixed_couple, -rel, -kidsn_hh17, -emplst6, -mlstat5, -livpart)
}

# Example usage:
# psid_model_data <- prepare_model_data(processed_data)
