# model_specifications.R
# Defines model specifications and formulas

# Function to generate model formulas with dynamic controls
generate_model_specifications <- function() {
  dependent_variable <- "hwork ~ "

  baseline_explanatory_variables <- c("wife_earns_more", "female_income_share")

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

  fixed_effects <- c(
    "cpf_pid",
    "wavey",
    "rstate"
  )

  # pooled OLS ----
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
      str_flatten(c(baseline_explanatory_variables, income_controls, non_income_controls), collapse = " + "),
      ")"
    )
  )

  pols_cubics <- str_flatten(
    c(
      dependent_variable,
      "hisp*(",
      str_flatten(c(baseline_explanatory_variables, income_cubic_controls, non_income_controls), collapse = " + "),
      ")"
    )
  )

  # fixed effects ----
  # TODO: I can actually simplify all of these by simply removing hisp using the - sign and *
  fe_baseline <- str_flatten(
    c(
      dependent_variable,
      str_flatten(baseline_explanatory_variables, collapse = " + "),
      " + ",
      str_flatten(str_c("hisp:", baseline_explanatory_variables), collapse = " + "),
      " | ",
      str_flatten(fixed_effects, collapse = " + ")
    )
  )

  fe_controls <- str_flatten(
    c(
      dependent_variable,
      str_flatten(c(baseline_explanatory_variables, income_controls, non_income_controls), collapse = " + "),
      " + ",
      str_flatten(str_c("hisp:", c(baseline_explanatory_variables, income_controls, non_income_controls), collapse = " + ")),
      " | ",
      str_flatten(fixed_effects, collapse = " + ")
    )
  )

  fe_cubics <- str_flatten(
    c(
      dependent_variable,
      str_flatten(c(baseline_explanatory_variables, income_cubic_controls, non_income_controls), collapse = " + "),
      " + ",
      str_flatten(str_c("hisp:", c(baseline_explanatory_variables, income_cubic_controls, non_income_controls), collapse = " + ")),
      " | ",
      str_flatten(fixed_effects, collapse = " + ")
    )
  )

  lst(pols_baseline, pols_controls, pols_cubics, fe_baseline, fe_controls, fe_cubics)
}

# Example usage:
# model_specs <- generate_model_specifications()
# fe_cubics <- model_specs$fe_cubics
# fe_formula <- model_specs$fe_formula
