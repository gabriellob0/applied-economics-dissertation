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
      estimates = str_c(sprintf("%.3f", estimate), "\n(", sprintf("%.3f", std.error), ")")
    )

  wide_tbl <- cleaned_coefs |>
    select(female, specification, term, estimates) |>
    pivot_wider(
      names_from = specification,
      values_from = estimates,
      values_fill = "-"
    ) |>
    group_by(female)
}


# Function to generate and style table from dataframe
generate_regression_table <- function(regression_tbl) {
  order_panels <- c("Panel A: Women", "Panel B: Men")

  basic_tbl <- regression_tbl |>
    arrange(factor(female, leve = order_panels)) |>
    gt(rowname_col = "term") |>
    tab_header(title = "Regression Table")
  
  basic_tbl |>
    cols_label(
      pols_baseline = "(1)",
      pols_controls = "(2)",
      pols_cubics = "(3)",
      fe_baseline = "(4)",
      fe_controls = "(5)",
      fe_cubics = "(6)"
    ) |>
    cols_align(
      align = "center",
      columns = starts_with(c("pols", "fe"))
    )
}


# Function to add additional information to table
add_specification_rows <- function(model_coef_tbl) {
  model_coef_tbl |>
    distinct(specification) |>
    mutate(
      `Controls` = if_else(str_detect(specification, "baseline"), "No", "Yes"),
      `Cubics` = if_else(str_detect(specification, "cubics"), "Yes", "No"),
      `Fixed Effects` = if_else(str_detect(specification, "fe"), "Yes", "No")
    ) |>
    pivot_longer(-specification, names_to = "term", values_to = "Value") |>
    pivot_wider(names_from = specification, values_from = Value) |>
    as.list()
}
