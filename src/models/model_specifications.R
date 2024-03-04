# model_specifications.R
# Defines model specifications and formulas

# Function to generate model formulas with dynamic controls
generate_model_specifications <- function() {
  dependent_variable <- "hwork ~ "
  baseline_explanatory_variable <- "wife_earns_more"

  # controls ----
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
  
  relative_income <- "female_income_share"
  
  # fixed effects ----
  pols_fixed_effects <- c("wavey", "rstate")
  individual_fixed_effects <- c("cpf_pid", "wavey", "rstate")
  
  
  # base formulas ----
  baseline <- str_flatten(
    c(
      dependent_variable,
      "hisp*(",
      str_flatten(c(baseline_explanatory_variable, income_controls, non_income_controls), collapse = " + "),
      ")"
    )
  )

  controls <- str_flatten(
    c(
      dependent_variable,
      "hisp*(",
      str_flatten(c(baseline_explanatory_variable, relative_income, income_controls, non_income_controls), collapse = " + "),
      ")"
    )
  )

  cubics <- str_flatten(
    c(
      dependent_variable,
      "hisp*(",
      str_flatten(c(baseline_explanatory_variable, relative_income, income_cubic_controls, non_income_controls), collapse = " + "),
      ")"
    )
  )

  # POLS ----
  pols_baseline <- str_flatten(
    c(
      baseline,
      " | ",
      str_flatten(pols_fixed_effects, collapse = " + ")
    )
  )
  
  pols_controls <- str_flatten(
    c(
      controls,
      " | ",
      str_flatten(pols_fixed_effects, collapse = " + ")
    )
  )
  
  pols_cubics <- str_flatten(
    c(
      cubics,
      " | ",
      str_flatten(pols_fixed_effects, collapse = " + ")
    )
  )
  
  # fixed effects ----
  fe_baseline <- str_flatten(
    c(
      baseline,
      " - hisp | ",
      str_flatten(individual_fixed_effects, collapse = " + ")
    )
  )

  fe_controls <- str_flatten(
    c(
      controls,
      " - hisp | ",
      str_flatten(individual_fixed_effects, collapse = " + ")
    )
  )

  fe_cubics <- str_flatten(
    c(
      cubics,
      " - hisp | ",
      str_flatten(individual_fixed_effects, collapse = " + ")
    )
  )

  lst(pols_baseline, pols_controls, pols_cubics, fe_baseline, fe_controls, fe_cubics)
}

# Example usage:
# model_specs <- generate_model_specifications()
# fe_cubics <- model_specs$fe_cubics
# fe_formula <- model_specs$fe_formula
