# model_specifications.R
# Defines model specifications and formulas

# Function to generate model formulas as string
generate_model_specifications <- function() {
  controls <- "log(incjob1_mg) + log(part_income) + log(hhinc_post) + edu4 + part_edu + poly(age, 2) + poly(part_age, 2) + kids + rstate"

  pols_formula <- str_flatten(c("hwork ~ wife_earns_more * hisp + female_income_share", controls), collapse = " + ")
  fe_formula <- str_flatten(c(pols_formula, "cpf_pid + wavey"), collapse = " | ")

  lst(
    pols_formula = pols_formula,
    fe_formula = fe_formula
  )
}

# Example usage:
# model_specs <- generate_model_specifications()
# pols_formula <- model_specs$pols_formula
# fe_formula <- model_specs$fe_formula
