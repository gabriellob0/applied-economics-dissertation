# model_specifications.R
# Defines model specifications and formulas

# Function to generate model formulas with dynamic controls
generate_model_specifications <- function() {
  dependent_variable <- "hwork ~ "
  
  baseline_explanatory_variables <- c("wife_earns_more", "female_income_share")
  
  non_income_controls <- c(
    "edu4",
    "part_edu",
    "poly(age, 2)",
    "poly(part_age, 2)",
    "kids"
  )
  
  income_controls <- c(
    "log(incjob1_mg)",
    "log(part_income)",
    "log(hhinc_post)"
  )
  
  income_cubic_controls <- c(
    "poly(log(incjob1_mg), 3)",
    "poly(log(part_income), 3)",
    "log(hhinc_post)"
  )
  
  fixed_effects <- c(
    "cpf_pid",
    "wavey",
    "rstate"
  )
  
  pols_baseline <- str_flatten(
    c(
      dependent_variable,
      "hisp*(",
      str_flatten(baseline_explanatory_variables, collapse = " + "),
      ")"
    )
  )
  
  pols_controls <- str_flatten(
    c(
      dependent_variable,
      "hisp*(",
      str_flatten(
        c(baseline_explanatory_variables, income_controls, non_income_controls),
        collapse = " + "
      ),
      ")"
    )
  )
  
  pols_cubics <- str_flatten(
    c(
      dependent_variable,
      "hisp*(",
      str_flatten(
        c(baseline_explanatory_variables, income_cubic_controls, non_income_controls),
        collapse = " + "
      ),
      ")"
    )
  )
  
  fe_baseline <- str_flatten(
    c(pols_baseline, " | ", str_flatten(fixed_effects, collapse = " + "))
  )
  
  fe_controls <- str_flatten(
    c(pols_controls, " | ", str_flatten(fixed_effects, collapse = " + "))
  )
  
  fe_cubics <- str_flatten(
    c(pols_cubics, " | ", str_flatten(fixed_effects, collapse = " + "))
  )
  
  lst(pols_baseline, pols_controls, pols_cubics, fe_baseline, fe_controls, fe_cubics)
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
