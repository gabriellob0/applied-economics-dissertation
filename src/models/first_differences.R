find_largest_household_dataset <- function(data) {
  years <- unique(data$wavey)
  years <- sort(years)

  largest_dataset <- NULL
  max_households <- 0

  for (i in 1:(length(years) - 1)) {
    year1 <- years[i]
    year2 <- years[i + 1]

    num_households <- data |>
      filter(wavey %in% c(year1, year2)) |>
      filter(n_distinct(cpf_pid) == 2, .by = c(cpf_hid, wavey)) |>
      filter(n_distinct(wavey) == 2, .by = cpf_hid) |>
      pull(cpf_hid) |>
      unique() |>
      length()

    if (num_households > max_households) {
      max_households <- num_households
      largest_dataset <- c(year1, year2)
    }
  }

  return(largest_dataset)
}

#print(find_largest_household_dataset(psid_model_data))
# 1994, 1995

prepare_first_diff <- function(data) {
  data |>
    filter(wavey %in% c(1994, 1995)) |>
    filter(n_distinct(cpf_pid) == 2, .by = c(cpf_hid, wavey)) |>
    filter(n_distinct(wavey) >= 2, .by = cpf_hid)
}
