# modeling.R
# Performs model estimations

# Function to estimate models for each group
estimate_models <- function(data_tbl, pols_formula, fe_formula) {
  data_tbl |>
    group_nest(female) |>
    mutate(
      pols = map(data, \(x) feols(as.formula(pols_formula), data = x)),
      fe = map(data, \(x) feols(as.formula(fe_formula), data = x))
    )
}

# Function to summarize model results
summarise_model_results <- function(models_tbl) {
  models_tbl |>
    pivot_longer(c(pols, fe), names_to = "specification", values_to = "model") |>
    mutate(coef = map(model, tidy)) |>
    select(female, specification, coef) |>
    unnest(cols = c(coef))
}


# Example usage:
# modeled_data <- estimate_models(psid_model_data, pols_formula, fe_formula)
# summarized_results <- summarize_model_results(modeled_data)
