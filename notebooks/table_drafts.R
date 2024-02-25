# draft ----
# Describe functions
# TODO: put into functions

#table_data <- reg_coefs |>
#  filter(term %in% c("wife_earns_more", "wife_earns_more:hisp", "hisp")) |>
#  mutate(
#    stars = case_when(
#      between(p.value, 0.05, 0.1) ~ "*",
#      between(p.value, 0.01, 0.05) ~ "**",
#      p.value < 0.01 ~ "***",
#      TRUE ~ ""
#    ),
#    coef_stats = str_c(estimate, stars, "<br>(", std.error, ")")
#  ) |>
#  select(female, specification, term, coef_stats) |>
#  pivot_wider(names_from = "specification", values_from = "coef_stats")
#
#
#reg_table <- table_data |>
#  gt(rowname_col = "term", groupname_col = "female") |>
#  tab_stubhead(label = "Panel") |>
#  fmt_markdown(columns = everything())

#reg_table

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

modelsummary(
  table_data,
  stars = TRUE,
  coef_omit = "^(?!(wife_earns_more|hisp|wife_earns_more.*hisp|hisp.*wife_earns_more)$).*$",
  coef_rename = c("hisp" = "Hispanic", "wife_earns_more" = "Wife Earns More")
)










# regression_tables.R

# This module provides a function to generate regression tables for fixed effects models
# using predefined specifications focused on specific coefficients with the 'fixest' package.


# Function to generate regression table with predefined parameters
generate_regression_table <- function(models_list) {
  # Predefined parameters
  dict_vars <- c(wife_earns_more = "Wife Earns More", hisp = "Hispanic", `wife_earns_more:hisp` = "Wife Earns More x Hispanic")
  keep_vars <- c("%^wife_earns_more$", "%^hisp$", "%^wife_earns_more:hisp$")
  order_vars <- c("wife_earns_more", "hisp", "wife_earns_more:hisp")
  table_title <- "Selected Coefficients from Fixed Effects Models"
  stars <- c("***" = 0.01, "**" = 0.05, "*" = 0.1)
  
  # Generation of the regression table
  etable(
    models_list,
    dict = dict_vars,
    keep = keep_vars,
    order = order_vars,
    title = table_title,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1)
  )
}

# Note: To use this function, ensure you provide a correctly structured `models_list`
# Example call (assuming `psid_models` is defined elsewhere in your code):
# models_flat <- c(psid_models$pols, psid_models$fe)
# generate_regression_table(models_flat)