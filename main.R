# dependencies ----
source("src/utils.R")
source("src/features/read_psid_data.R")
source("src/features/query.R")
source("src/features/prepare_model_data.R")
source("src/models/model_specifications.R")
source("src/models/modeling.R")
source("src/visualisation/regression_tables.R")

required_packages <- c(
  "arrow",
  "duckdb",
  "dplyr",
  "fixest",
  "stringr",
  "purrr", # TODO: consider furrr for parallel
  "broom",
  "tidyr",
  "gt"
)

load_packages(required_packages)


# import data ----
# TODO: consider cbirth and immiyear
psid_fpath <- "data/interim/psid_hufe.arrow"
psid <- read_psid_data(psid_fpath)


# db connection ----
con <- dbConnect(duckdb(), dbdir = ":memory:")
duckdb_register(con, "psid", psid)


# query ----
psid_query <- perform_data_filtering(con, "psid") |>
  group_and_mutate_data()


# processing ----
psid_queried <- collect(psid_query)
psid_model_data <- prepare_model_data(psid_queried)
# write_feather(psid_model_data, "data/processed/model_data.arrow")
# psid_model_data <- arrow::read_feather("data/processed/model_data.arrow")


# specifications ----
model_formulas <- generate_model_specifications()


# modelling ----
# TODO: need to think about the standard errors
psid_models <- estimate_models(psid_model_data, model_formulas)

model_estimates <- tidy_model_estimates(psid_models)
model_statistics <- psid_models |>
  generate_model_statistics() |>
  format_model_stats()


# tables ----
model_descriptions <- generate_specification_rows(model_estimates)

reg_table <- model_estimates |>
  prepare_regression_table() |>
  generate_regression_table(model_formulas) |>
  rows_add(.list = model_statistics) |>
  rows_add(.list = model_descriptions) |>
  style_regression_table() |>
  opt_table_font(font = "Libertinus Serif Semibold") |>
  tab_options(table.width = pct(70))#, table.font.size = px(10))

reg_table

#gtsave(reg_table, "reporting/regression_table.png", expand = 100)


# plots ----
library(ggplot2)

female_density <- psid_model_data |>
  filter(female == 1)

non_hisp_density <- ggplot(aes(x = female_income_share), data = filter(female_density, hisp == 0)) +
  geom_density() +
  theme_bw()

hisp_density <- ggplot(aes(x = female_income_share), data = filter(female_density, hisp == 1)) +
  geom_density() +
  theme_bw()

gridExtra::grid.arrange(hisp_density, non_hisp_density, ncol = 2)
