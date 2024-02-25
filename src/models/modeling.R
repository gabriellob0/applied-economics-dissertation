# modeling.R
# Performs model estimations

# Function to estimate models for each group
estimate_models <- function(data_tbl, specifications) {
  spec_tbl <- tibble::enframe(
    specifications,
    name = "specification", value = "formula"
  )

  data_tbl |>
    group_nest(female) |>
    expand_grid(spec_tbl) |>
    mutate(
      estimated_models = map2(
        formula, data, \(x, y) feols(as.formula(x), data = y)
      )
    )
}

# Function to summarize model results
summarise_model_results <- function(models_tbl) {
  models_tbl |>
    mutate(coef = map(estimated_models, tidy)) |>
    select(female, specification, coef) |>
    unnest(cols = c(coef))
}


# Example usage:
# modeled_data <- estimate_models(psid_model_data, formulas)
# summarised_results <- summarise_model_results(modeled_data)
