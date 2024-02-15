# dependencies ----
source("src/load_packages.R")
source("src/data_import.R")
source("src/query_processing.R")
source("src/data_processing.R")

required_packages <- c(
  "arrow",
  "duckdb",
  "dplyr",
  "fixest",
  "stringr",
  "purrr",
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


# specifications ----
model_formulas <- generate_model_specifications()


# modelling ----
psid_models <- psid_model_data |>
  group_nest(female) |>
  mutate(
    pols = map(data, \(x) lm(as.formula(pols_formula), data = x)),
    fe = map(data, \(x) feols(as.formula(fe_formula), data = x))
  )

summary(psid_models[[4]][[2]], vcov = "twoway")

reg_coef <- psid_models |>
  pivot_longer(c(pols, fe), names_to = "specification", values_to = "model") |>
  mutate(coef = map(model, tidy)) |>
  select(female, specification, coef) |>
  unnest(cols = c(coef))


# tables ----
table_data <- reg_coef |>
  filter(term %in% c("wife_earns_more", "wife_earns_more:hisp", "hisp")) |>
  mutate(
    stars = case_when(
      between(p.value, 0.05, 0.1) ~ "*",
      between(p.value, 0.01, 0.05) ~ "**",
      p.value < 0.01 ~ "***",
      TRUE ~ ""
    ),
    coef_stats = str_c(estimate, stars, "<br>(", std.error, ")")
  ) |>
  select(female, specification, term, coef_stats) |>
  pivot_wider(names_from = "specification", values_from = "coef_stats")


reg_table <- table_data |>
  gt(rowname_col = "term", groupname_col = "female") |>
  tab_stubhead(label = "Panel") |>
  fmt_markdown(columns = everything())

#TODO: it is estimating stuff for hisp in my FE model, which should be impossible
psid_model_data |>
  filter(female == 1) |>
  group_by(cpf_pid, wavey) |>
  filter(length(unique(hisp)) != 1)

psid_model_data %>%
  group_by(cpf_pid) %>%
  summarize(num_unique_hisp = n_distinct(hisp)) %>%
  summarize(max_num_unique_hisp = max(num_unique_hisp))
