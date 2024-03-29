# plots.R

# Functions to prepare density plots and analyse discontinuities in the data.

clean_manipulation_data <- function(data_tbl) {
  clean_tbl <- select(data_tbl, cpf_hid, wavey, female_income_share, hisp)

  full_sample <- clean_tbl |>
    mutate(hisp = "full-sample")

  income_tbl <- clean_tbl |>
    mutate(hisp = if_else(hisp == 1, "hispanic", "non-hispanic")) |>
    bind_rows(full_sample) |>
    group_by(cpf_hid, hisp) |>
    filter(wavey == min(wavey)) |>
    ungroup() |>
    distinct()

  lst(
    hispanic = income_tbl |>
      filter(hisp == "hispanic") |>
      pull(female_income_share),
    `non-hispanic` = income_tbl |>
      filter(hisp == "non-hispanic") |>
      pull(female_income_share),
    `full-sample` = income_tbl |>
      filter(hisp == "full-sample") |>
      pull(female_income_share)
  )
}

test_manipulation <- function(manipulation_lst, CUTOFF = 0.5) {
  manipulation_lst |>
    map(\(x) rddensity(x, CUTOFF))
}

create_discontinuity_plot <- function(manipulation_lst, CUTOFF = 0.5) {
  manipulation_lst |>
    map(\(x) rdplotdensity(rddensity(x, CUTOFF), x))
}

plot_discontinuity <- function(discontinuity_lst, CUTOFF = 0.5) {
  max_y <- 8
  
  discontinuity_lst |>
    imap(~ {
      left_data <- as.data.frame(.x$Estl$Estimate)
      right_data <- as.data.frame(.x$Estr$Estimate)
      
      ggplot() +
        theme_bw() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text = element_text(family = "Libertinus Serif", size = 12, face = "bold"),
          axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 16),
          plot.subtitle = element_text(hjust = 0.5),
          axis.ticks.y = element_blank(),
          axis.text = element_text(size = 12),
          plot.caption = element_text(hjust = 0.5, margin = margin(t = 10))
        ) +
        geom_line(data = left_data, aes(x = grid, y = f_q), col = "black", linewidth = 0.8) +
        geom_line(data = right_data, aes(x = grid, y = f_q), col = "black", linewidth = 0.8) +
        geom_vline(xintercept = CUTOFF, linetype = "dashed", color = "gray30") +
        labs(
          x = "Share of Income Earned by the Wife",
          y = "Relative Estimated Density"
        ) +
        coord_cartesian(xlim = c(0, 1), ylim = c(0, max_y))
    })
}
