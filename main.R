# dependencies ----
# Note you need the AGG backend + Libertinus Serif for the charts to render
# properly. It might still work even without modifications,
# but it might just not look the same.
source("src/utils.R")
source("src/features/read_psid_data.R")
source("src/features/query.R")
source("src/features/prepare_model_data.R")
source("src/models/model_specifications.R")
source("src/models/modeling.R")
source("src/models/first_differences.R")
source("src/visualisation/summary_tables.R")
source("src/visualisation/discontinuity_plots.R")
source("src/visualisation/regression_tables.R")

required_packages <- c(
  "arrow",
  "duckdb",
  "dplyr",
  "fixest",
  "stringr",
  "purrr", # TODO: consider furrr for parallel
  "broom",
  "tidyr",
  "gt",
  "rddensity",
  "ggplot2",
  "cowplot"
)

load_packages(required_packages)


# import data ----
# TODO: consider cbirth and immiyear
psid_fpath <- "data/interim/psid_hufe.arrow"
psid <- read_psid_data(psid_fpath)


# db connection ----
con <- dbConnect(duckdb(), dbdir = ":memory:")
duckdb_register(con, "psid", psid)


# query ----
psid_query <- con |>
  perform_data_filtering("psid") |>
  group_and_mutate_data()


# processing ----
psid_queried <- collect(psid_query)
psid_model_data <- prepare_model_data(psid_queried)
# write_feather(psid_model_data, "data/processed/model_data.arrow")
# psid_model_data <- arrow::read_feather("data/processed/model_data.arrow")


# summary statistics ----
data_table <- psid_model_data |>
  summarise_model_data() |>
  style_summary_data() |>
  opt_table_font(font = "Libertinus Serif Semibold") |>
  tab_options(table.width = pct(70))

#gtsave(data_table, "reporting/tables/data_table.png", expand = 100)


# manipulation testing ----
manipulation_data <- clean_manipulation_data(psid_model_data)
summary(test_manipulation(manipulation_data)$hispanic)
summary(test_manipulation(manipulation_data)$`non-hispanic`)
summary(test_manipulation(manipulation_data)$`full-sample`)

discontinuity_plots <- manipulation_data |>
  create_discontinuity_plot() |>
  plot_discontinuity()

figure1 <- discontinuity_plots$`full-sample` +
  labs(title = "Figure 1",
       subtitle = "Distribution of Relative Income (PSID) for the full sample (Hispanics and non-Hispanics)")

hisp_plot <- discontinuity_plots$`hispanic` +
  labs(title = "Hispanic Households")

non_hisp_plot <- discontinuity_plots$`non-hispanic` +
  labs(title = "non-Hispanic Households")

combined_plot <- plot_grid(
  hisp_plot, non_hisp_plot, ncol = 2
)

title_gg <- ggplot() + 
  labs(title = "Figure 2", subtitle = "Distribution of Relative Income (PSID)") +
  theme(text = element_text(family = "Libertinus Serif", face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

figure2 <- plot_grid(
  title_gg, combined_plot,
  ncol = 1, rel_heights = c(0.1, 1)
)

#ggsave("reporting/figures/figure1.png", figure1)
#ggsave("reporting/figures/figure2.png", figure2)


# specifications ----
model_formulas <- generate_model_specifications()


# modelling ----
psid_models <- estimate_models(psid_model_data, model_formulas)

model_estimates <- tidy_model_estimates(psid_models)

model_statistics <- psid_models |>
  generate_model_statistics() |>
  format_model_stats()


# tables ----
model_descriptions <- generate_specification_rows(model_estimates)

reg_table <- model_estimates |>
  prepare_regression_table() |>
  generate_regression_table(model_formulas) |>
  rows_add(.list = model_statistics) |>
  rows_add(.list = model_descriptions) |>
  style_regression_table() |>
  opt_table_font(font = "Libertinus Serif Semibold") |>
  tab_options(table.width = pct(70))#, table.font.size = px(10))

reg_table

#gtsave(reg_table, "reporting/tables/regression_table.png", expand = 100)


# first-differences ----
first_diff_data <- psid_model_data |>
  prepare_first_diff() |>
  estimate_models(model_formulas) |>
  tidy_model_estimates() |>
  prepare_regression_table() |>
  generate_regression_table(model_formulas)
