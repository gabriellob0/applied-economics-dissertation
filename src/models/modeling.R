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
      estimated_model = map2(
        formula, data, \(x, y) feols(as.formula(x), data = y)
      ),
      nhouseholds = map(
        data, \(x) length(unique(pull(x, cpf_hid)))
      )
    )
}

# Function to tidy model estimates
tidy_model_estimates <- function(models_tbl) {
  models_tbl |>
    mutate(coef = map(estimated_model, tidy)) |>
    select(female, specification, coef) |>
    unnest(cols = c(coef))
}

# Function to summarise model
# TODO: consider if possible to go in fn. above.
generate_model_statistics <- function(models_tbl) {
  models_tbl |>
    mutate(model_stats = map(estimated_model, glance), nhouseholds = as.numeric(nhouseholds)) |>
    unnest(cols = c(model_stats)) |>
    select(female, specification, adj.r.squared, within.r.squared, nobs, nhouseholds)
}

# Example usage:
# TODO: fix examples
# modeled_data <- estimate_models(psid_model_data, formulas)
# summarised_results <- summarise_model_results(modeled_data)
