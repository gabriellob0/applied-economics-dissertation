# library(haven)
library(arrow)
library(duckdb)
library(dplyr)
library(fixest)

psid <- read_feather("data/raw/psid_hufe.arrow")

con <- dbConnect(duckdb(), dbdir = ":memory:")
duckdb_register(con, "psid", psid, overwrite = TRUE)

clean <- tbl(con, "psid") |>
  select(cpf_pid, cpf_hid, wave, rel, nphh, female, hisp, emplst6, incjob1_mg, hwork) |>
  filter(
    if_all(everything(), \(x) !is.na(x)),
    incjob1_mg > 0,
    rel %in% c(1, 2),
    nphh > 1,
    emplst6 == 1
  )

coded <- clean |>
  group_by(cpf_hid, wave) |>
  filter(n() == 2, sum(female) == 1) |>
  mutate(
    income_share = incjob1_mg / sum(incjob1_mg),
    wife_earns_more = case_when(
      female == 1 & income_share >= 0.5 ~ 1,
      female == 0 & income_share < 0.5 ~ 1,
      TRUE ~ 0
    )
  ) |>
  ungroup()

test <- collect(coded)

females <- filter(test, female == 1)

pols_model <- lm(hwork ~ wife_earns_more*hisp, females)
fe_model <- feols(hwork ~ wife_earns_more*hisp | wave + cpf_pid, females)

summary(fe_model)

# psid <- read_dta("data//raw//psid_hufe.dta")
# write_feather(psid, "data/raw/psid_hufe.arrow")
