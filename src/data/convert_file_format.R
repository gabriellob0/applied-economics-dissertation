# library(haven)
psid <- read_dta("data/raw/psid_hufe.dta")
write_feather(psid, "data/interim/psid_hufe.arrow")
