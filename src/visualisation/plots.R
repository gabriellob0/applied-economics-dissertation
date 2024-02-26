library(dplyr)
library(purrr)

test <- psid_model_data |>
  filter(female == 1) |>
  group_nest(hisp) |>
  mutate(female_income_share = map(data, \(x) pull(x, female_income_share)))

map(test$female_income_share, \(x) plot(density(x)))
