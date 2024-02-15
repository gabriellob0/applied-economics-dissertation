# draft 1 ----
testing_data <- psid_model_data |>
  filter(female == 1)

lm_pols_model <- lm(as.formula(pols_formula), testing_data)
fixest_pols_model <- feols(as.formula(pols_formula), testing_data)

summary(lm_pols_model)
summary(fixest_pols_model)

new_for <- "hwork ~ wife_earns_more*hisp + female_income_share + log(incjob1_mg) + log(part_income) + log(hhinc_post) + edu4 + part_edu + poly(age, 2) + poly(part_age, 2) + kids | rstate"
new_forfe <- "hwork ~ wife_earns_more*hisp + female_income_share + log(incjob1_mg) + log(part_income) + log(hhinc_post) + edu4 + part_edu + poly(age, 2) + poly(part_age, 2) + kids | cpf_pid + wavey + rstate"

fixest_pols_model2 <- feols(as.formula(new_for), testing_data, vcov = twoway ~ cpf_pid + wavey)

summary(fixest_pols_model2)

fixest_split <- feols(as.formula(new_forfe), psid_model_data, split = ~ female)
summary(fixest_split, vcov = "twoway")

etable(fixest_split)


# draft 2 ----
