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
  "purrr", #TODO: consider furrr for parallel
  #"broom",
  "tidyr"#,
  #"gt"
)

load_packages(required_packages)


# import data ----
psid_fpath <- "data/interim/psid_hufe.arrow"

# TODO: consider cbirth and immiyear
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
#write_feather(psid_model_data, "data/processed/model_data.arrow")

# specifications ----
model_formulas <- generate_model_specifications()


# modelling ----
#TODO: need to think about the standard errors
psid_models <- estimate_models(psid_model_data, model_formulas)


# tables ----
#config_modelsummary(factory_default = 'tinytable')

table_data <- psid_models |>
  filter(female == 1) |>
  mutate(
    panel = if_else(female == 1, "Female", "Male"),
    title = str_replace_all(specification, "_", " "),
    title = str_c(
      str_to_upper(str_extract(title, "^[^ ]+")), str_to_title(str_extract(title, " [^ ]+$")), sep = " "
    )
  ) |>
  select(title, estimated_models) |>
  tibble::deframe()

modelsummary(table_data, gof_omit = "All", coef_omit = "^(?!^(hisp|wife_earns_more|hisp:wife_earns_more)$).*$")
