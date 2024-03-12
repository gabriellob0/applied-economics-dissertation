# summary_tables.R

# Functions to prepare and create statistical summaries of the data

# Function to aggregate and pivot data summary statistics for main regression
# sample.
summarise_model_data <- function(data_tbl) {
  totals_tbl <- data_tbl |>
    select(cpf_pid, cpf_hid, wavey, female, hisp, hwork, wife_earns_more) |>
    summarise(
      housework = mean(hwork),
      wife_earns_more_pct = mean(wife_earns_more),
      observations = n(),
      individuals = n_distinct(cpf_pid),
      households = n_distinct(cpf_hid)
    )

  agg_tbl <- data_tbl |>
    select(cpf_pid, cpf_hid, wavey, female, hisp, hwork, wife_earns_more) |>
    group_by(female, hisp) |>
    summarise(
      housework = mean(hwork),
      wife_earns_more_pct = mean(wife_earns_more),
      observations = n(),
      individuals = n_distinct(cpf_pid),
      households = n_distinct(cpf_hid)
    ) |>
    bind_rows(totals_tbl)

  pivoted_tbl <- agg_tbl |>
    pivot_longer(
      cols = c(housework, wife_earns_more_pct, observations, individuals, households),
      names_to = "variable",
      values_to = "value"
    ) |>
    pivot_wider(
      names_from = c(female, hisp),
      values_from = value,
      names_glue = "{female}_{hisp}"
    ) |>
    rename(total = "NA_NA")
}

style_summary_data <- function(summary_tbl) {
  complete_table <- summary_tbl |>
    mutate(variable = case_when(
      variable == "housework" ~ "Average Housework (hrs / wk)",
      variable == "wife_earns_more_pct" ~ "Percentage of Wives Earning More",
      variable == "observations" ~ "Number of Observations",
      variable == "individuals" ~ "Number of Individuals",
      variable == "households" ~ "Number of Households",
    )) |>
    gt(rowname_col = "variable")

  complete_table |>
    tab_header(
      title = "Table I",
      subtitle = "Descriptive Statistics by Gender and Hispanic Origin"
    ) |>
    tab_spanner(
      label = "Men",
      columns = c(`0_0`, `0_1`)
    ) |>
    tab_spanner(
      label = "Women",
      columns = c(`1_0`, `1_1`)
    ) |>
    cols_label(
      `0_0` = "Non-Hispanic",
      `0_1` = "Hispanic",
      `1_0` = "Non-Hispanic",
      `1_1` = "Hispanic",
      `total` = "Full Sample"
    ) |>
    cols_align(
      align = "center",
      columns = c(`0_0`, `0_1`, `1_0`, `1_1`, `total`)
    ) |>
    fmt_number(
      columns = c(`0_0`, `0_1`, `1_0`, `1_1`, `total`),
      decimals = 2,
      rows = variable == "Average Housework (hrs / wk)"
    ) |>
    fmt_percent(
      columns = c(`0_0`, `0_1`, `1_0`, `1_1`, `total`),
      decimals = 2,
      rows = variable == "Percentage of Wives Earning More"
    ) |>
    fmt_integer(
      columns = c(`0_0`, `0_1`, `1_0`, `1_1`, `total`),
      rows = !(variable %in% c("Average Housework (hrs / wk)", "Percentage of Wives Earning More"))
    ) |>
    tab_source_note(
      source_note = md(
        "Notes: Data from the Panel Study of Income Dynamics (PSID) from 1990 to 2019. The sample contains individual-level data linked over multiple years and only includes cohabiting dual-earner married couples aged between 18 and 65. Couples with mixed ethnic backgrounds (i.e., one Hispanic and one non-Hispanic person) are excluded. All observations where any of the regression variables are missing are also excluded (see Empirical Strategy section for details). Housework hours are measured in hours per weekday. Samples between male and female non-Hispanic individuals do not match due to changes in households or remarriage."
      )
    )
}
