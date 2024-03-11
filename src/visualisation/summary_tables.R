# summary_tables.R

# Functions to prepare and create statistical summaries of the data

# Function to aggregate and pivot data summary statistics for main regression
# sample.
summarise_model_data <- function(data_tbl) {
  agg_tbl <- data_tbl |>
    select(cpf_pid, cpf_hid, wavey, female, hisp, hwork) |>
    group_by(female, hisp) |>
    summarise(
      housework = mean(hwork),
      observations = n(),
      individuals = n_distinct(cpf_pid),
      households = n_distinct(cpf_hid)
    )

  pivoted_tbl <- agg_tbl |>
    pivot_longer(
      cols = c(housework, observations, individuals, households),
      names_to = "variable",
      values_to = "value"
    ) |>
    pivot_wider(
      names_from = c(female, hisp),
      values_from = value,
      names_glue = "{female}_{hisp}"
    )
}

style_summary_data <- function(summary_tbl) {
  complete_table <- summary_tbl |>
    mutate(variable = case_when(
      variable == "housework" ~ "Average Housework (hrs / wk)",
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
      `1_1` = "Hispanic"
    ) |>
    cols_align(
      align = "center",
      columns = c(`0_0`, `0_1`, `1_0`, `1_1`)
    ) |>
    fmt_number(
      columns = c(`0_0`, `0_1`, `1_0`, `1_1`),
      decimals = 2,
      rows = variable == "Average Housework (hrs / wk)"
    ) |>
    fmt_integer(
      columns = c(`0_0`, `0_1`, `1_0`, `1_1`),
      rows = variable != "Average Housework (hrs / wk)"
    ) |>
    tab_source_note(
      source_note = md(
        "Notes: Sample includes married couples aged 18 to 65 year old. Only couples in which both members are employed and have positive income in a given year are included. Some additional text so I can test when this guy will breakline without me having to manually specify it Standard errors are reported in parenthesis."
      )
    )
}
