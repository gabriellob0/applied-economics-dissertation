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
        "Notes: Data from the Panel Study of Income Dynamics (PSID) from 1990 to 2019. The sample contains individual-level data linked over multiple years and only includes cohabiting dual-earner married couples aged between 18 and 65. Couples with mixed ethnic backgrounds (i.e., one Hispanic and one non-Hispanic person) are excluded. All observations where any of the regression variables are missing are also excluded (see Empirical Strategy section for details). Housework hours are measured in hours per weekday. **femaleRelativeIncome** is calculated as female income (gross, monthly) over the sum of female and male income (gross, monthly. **wifeEarnsMore** and *hisp* are indicator variables equal to one whenever the **femaleRelativeIncome** is larger than 50% and the individual is Hispanic, respectively. All specifications control for log of income (gross, monthly), log of partner income (gross, monthly), log of household income (post-tax), age, age squared, partner age, partner age squared, education, partner education, an indicator variable for children under 17 in the household, and state and time fixed effects. Additional controls include cubics of the log of incomes and individual fixed effects. Panel A is the female subsample and panel B is the male subsample. Standard errors are reported in parenthesis and clustered at the individual level. ***significant at 1% level, **at 5%, *at 10%."
      )
    )
}

# Function to add additional information to table
generate_specification_rows <- function(model_coef_tbl) {
  model_coef_tbl |>
    distinct(specification) |>
    mutate(
      `Relative Income` = if_else(str_detect(specification, "baseline"), "No", "Yes"),
      `Cubics` = if_else(str_detect(specification, "cubics"), "Yes", "No"),
      `Individual Fixed Effects` = if_else(str_detect(specification, "fe"), "Yes", "No"),
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
