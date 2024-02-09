# dependencies ----
library(arrow)
library(duckdb)
library(dplyr)
library(fixest)
library(stringr)
library(purrr)
library(broom)
library(tidyr)

psid_fpath <- "data/raw/psid_hufe.arrow"
#colnames(read_feather(psid_fpath))

#TODO: consider cbirth and immiyear
psid <- read_feather(
  psid_fpath,
  col_select = c(
    cpf_pid, cpf_hid, wave, wavey, rel, female, age, edu4, kidsn_hh17, nphh, emplst6, incjob1_mg, hhinc_post, hisp, hwork, rstate
  )
)


# db connection ----
con <- dbConnect(duckdb(), dbdir = ":memory:")
duckdb_register(con, "psid", psid)


# query ----
psid_filter_qries <- tbl(con, "psid") |>
  filter(
    if_all(everything(), \(x) !is.na(x)),
    rel %in% c(1, 2),
    between(age, 18, 65),
    edu4 > 0,
    nphh > 1,
    emplst6 == 1,
    incjob1_mg > 0,
    hhinc_post > 0
  )

psid_group_qries <- psid_filter_qries |>
  group_by(cpf_hid, wave) |>
  filter(n() == 2, sum(female) == 1) |>
  mutate(
    part_income = sum(incjob1_mg) - incjob1_mg,
    female_income_share = if_else(
      female == 1, incjob1_mg / sum(incjob1_mg), part_income / sum(incjob1_mg)
    ),
    wife_earns_more = if_else(female_income_share >= 0.5, 1, 0),
    part_age = sum(age) - age,
    part_edu = sum(edu4) - edu4,
    mixed_couple = if_else(sum(hisp) == 1, 1, 0),
    kids = if_else(kidsn_hh17 > 0, 1, 0)
  ) |>
  ungroup()


# processing ----
psid_qry <- collect(psid_group_qries)

psid_model_data <- psid_qry |>
  mutate(edu4 = as.factor(edu4), part_edu = as.factor(part_edu), rstate = as.factor(rstate)) |>
  filter(mixed_couple == 0) |>
  select(-mixed_couple, -rel, -kidsn_hh17, -nphh, -emplst6) |>
  group_nest(female)


# models ----
controls <- "log(incjob1_mg) + log(part_income) + log(hhinc_post) + edu4 + part_edu + poly(age, 2) + poly(part_age, 2) + kids + rstate"

pols_formula <- str_flatten(c("hwork ~ (wife_earns_more + hisp)^2 + female_income_share", controls), collapse = " + ")
fe_formula <- str_flatten(c(pols_formula, "cpf_pid + wavey"), collapse = " | ")


# draft ----
psid_models <- psid_model_data |>
  mutate(
    pols = map(data, \(x) lm(as.formula(pols_formula), data = x)),
    fe = map(data, \(x) feols(as.formula(fe_formula), data = x))
  )

test <- psid_models |>
  pivot_longer(!c(female,data), names_to = "specification", values_to = "model") |>
  mutate(coef = map(model, tidy)) |>
  select(female, specification, coef) |>
  unnest(cols = c(coef))