# regression_tables.R

# This module provides a function to generate regression tables for fixed effects models
# using predefined specifications focused on specific coefficients with the 'fixest' package.


# Function to prepare dataframe for table creation
prepare_regression_table <- function(model_coef_tbl) {
  cleaned_coefs <- model_coef_tbl |>
    filter(term %in% c("wife_earns_more", "hisp:wife_earns_more", "hisp")) |>
    mutate(
      term = case_when(
        term == "wife_earns_more" ~ "WifeEarnsMore",
        term == "hisp" ~ "Hispanic",
        term == "hisp:wife_earns_more" ~ "Hispanic x WifeEarnsMore"
      ),
      female = if_else(female == 1, "Panel A: Women", "Panel B: Men"),
      across(c(estimate, std.error), \(x) format(round(x, 3), nsmall = 3)),
      estimate = case_when(
        p.value < 0.1 & p.value >= 0.05 ~ str_c(as.character(estimate), "*"),
        p.value < 0.05 & p.value >= 0.01 ~ str_c(as.character(estimate), "**"),
        p.value < 0.01 ~ str_c(as.character(estimate), "***"),
        TRUE ~ as.character(estimate)
      )
    )

  wide_tbl <- cleaned_coefs |>
    select(female, specification, term, estimate, std.error) |>
    pivot_wider(
      names_from = specification,
      values_from = c(estimate, std.error)
    ) |>
    group_by(female)
}


# Function to generate table from dataframe
generate_regression_table <- function(regression_tbl, formulas) {
  order_panels <- c("Panel A: Women", "Panel B: Men")

  column_pairs <- names(formulas) |>
    map(\(x) c(str_c("estimate_", x), str_c("std.error_", x)))

  basic_tbl <- regression_tbl |>
    mutate(across(everything(), as.character)) |>
    arrange(factor(female, level = order_panels)) |>
    gt(rowname_col = "term")

  for (pair in column_pairs) {
    basic_tbl <- basic_tbl |>
      cols_merge(
        columns = pair,
        pattern = "<<{1}<br>({2})>>"
      )
  }

  return(basic_tbl)
}

# Function to style regression table
style_regression_table <- function(regression_tbl) {
  regression_tbl |>
    tab_header(
      title = "Table II",
      subtitle = "Violating Gender Norm and Household Production Across Hispanics and Non-Hispanics"
    ) |>
    sub_missing() |>
    cols_label(
      estimate_pols_baseline = "(1)",
      estimate_pols_controls = "(2)",
      estimate_pols_cubics = "(3)",
      estimate_fe_baseline = "(4)",
      estimate_fe_controls = "(5)",
      estimate_fe_cubics = "(6)"
    ) |>
    cols_align(
      align = "center",
      columns = starts_with("estimate")
    ) |>
    tab_source_note(
      source_note = md(
        "Notes: Sample includes married couples aged 18 to 65 year old. Only couples in which both members are employed and have positive income in a given year are included. Some additional text so I can test when this guy will breakline without me having to manually specify it Standard errors are reported in parenthesis. ***significant at 1% level, **at 5%, *at 10%."
      )
    )
}

# Function to add additional information to table
generate_specification_rows <- function(model_coef_tbl) {
  model_coef_tbl |>
    distinct(specification) |>
    mutate(
      `Controls` = if_else(str_detect(specification, "baseline"), "No", "Yes"),
      `Cubics` = if_else(str_detect(specification, "cubics"), "Yes", "No"),
      `Fixed Effects` = if_else(str_detect(specification, "fe"), "Yes", "No"),
      specification = str_c("estimate_", specification)
    ) |>
    pivot_longer(-specification, names_to = "term", values_to = "value") |>
    pivot_wider(names_from = specification, values_from = value) |>
    as.list()
}

# Function to format model statistics for gt table
format_model_stats <- function(model_stats_tbl) {
  model_stats_pivoted <- model_stats_tbl |>
    mutate(
      specification = str_c("estimate_", specification),
      across(c("adj.r.squared", "within.r.squared"), \(x) format(round(x, 3), nsmall = 3)),
      across(everything(), as.character)
    ) |>
    pivot_longer(adj.r.squared:nhouseholds) |>
    mutate(value = if_else(str_detect(value, "NA"), NA, value)) |>
    pivot_wider(names_from = specification, values_from = value)

  model_stats_pivoted |>
    rename(term = name) |>
    mutate(
      female = if_else(female == 1, "Panel A: Women", "Panel B: Men"),
      term = case_when(
        term == "adj.r.squared" ~ "Adj. R-Squared",
        term == "within.r.squared" ~ "Within R-Squared",
        term == "nobs" ~ "Observations",
        term == "nhouseholds" ~ "Households"
      )
    ) |>
    as.list()
}
