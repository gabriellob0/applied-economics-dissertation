# regression_tables.R
# Generate regression tables

# Describe functions
#TODO: put into functions

table_data <- reg_coef |>
  filter(term %in% c("wife_earns_more", "wife_earns_more:hisp", "hisp")) |>
  mutate(
    stars = case_when(
      between(p.value, 0.05, 0.1) ~ "*",
      between(p.value, 0.01, 0.05) ~ "**",
      p.value < 0.01 ~ "***",
      TRUE ~ ""
    ),
    coef_stats = str_c(estimate, stars, "<br>(", std.error, ")")
  ) |>
  select(female, specification, term, coef_stats) |>
  pivot_wider(names_from = "specification", values_from = "coef_stats")


reg_table <- table_data |>
  gt(rowname_col = "term", groupname_col = "female") |>
  tab_stubhead(label = "Panel") |>
  fmt_markdown(columns = everything())