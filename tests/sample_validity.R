# Verify no variation in `hisp` within `cpf_pid`
test_variation_in_hisp <- function(model_data) {
  rows <- model_data |>
    group_by(cpf_pid) |>
    summarise(variation_in_hisp = n_distinct(hisp)) |>
    filter(variation_in_hisp > 1) |>
    nrow()
  
  rows == 0
}
