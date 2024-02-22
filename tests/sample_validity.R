# Verify no variation in `hisp` within `cpf_pid`
psid_model_data |>
  group_by(cpf_pid) %>%
  summarise(variation_in_hisp = n_distinct(hisp)) |>
  filter(variation_in_hisp > 1)