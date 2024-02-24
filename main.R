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
  "broom",
  "tidyr",
  "gt"
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
generate_regression_table(psid_models$estimated_models[[1]])
etable(psid_models$estimated_models[[12]])


#reg_coefs <- summarise_model_results(psid_models)


# tables ----
# TODO: it is estimating stuff for hisp in my FE model, which should be impossible
psid_model_data |>
  filter(female == 1) |>
  group_by(cpf_pid, wavey) |>
  filter(length(unique(hisp)) != 1)

psid_model_data %>%
  group_by(cpf_pid) %>%
  summarize(num_unique_hisp = n_distinct(hisp)) %>%
  summarize(max_num_unique_hisp = max(num_unique_hisp))
