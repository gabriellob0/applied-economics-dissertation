# model_specifications.R
# Defines model specifications and formulas

# Function to generate model formulas with dynamic controls
generate_model_specifications <- function(controls = c(
                                            "female_income_share",
                                            "log(incjob1_mg)",
                                            "log(part_income)",
                                            "log(hhinc_post)",
                                            "edu4",
                                            "part_edu",
                                            "poly(age, 2)",
                                            "poly(part_age, 2)",
                                            "kids",
                                            "rstate"
                                          )) {
  controls_flattened <- str_flatten(controls, collapse = " + ")

  pols_formula <- str_flatten(c("hwork ~ wife_earns_more * hisp", controls_flattened), collapse = " + ")
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

# Example usage with custom controls:
# custom_controls <- c("female_income_share", "log(hhinc_post)", "edu4", "kids")
# custom_model_specs <- generate_model_specifications(controls = custom_controls)
# custom_pols_formula <- custom_model_specs$pols_formula
# custom_fe_formula <- custom_model_specs$fe_formula
