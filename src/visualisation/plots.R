# Load required libraries
library(rddensity)
library(ggplot2)
# Ensure other required libraries like dplyr and purrr are loaded

# Function to prepare plot data based on input data and a specified cutoff
# It filters the data for the minimum wavey value per cpf_hid and calculates
# density estimates for plotting.
# Inputs:
# - data: A dataframe containing at least cpf_hid, wavey, and female_income_share columns
# - cutoff: A numeric value indicating the cutoff point for density calculation
# Outputs:
# - A list containing data frames for left and right side plots including confidence intervals
prepare_plot_data <- function(data, cutoff) {
  # Filter the data for initial observations per cpf_hid and pull the female_income_share column
  plot_data <- data |> 
    dplyr::select(cpf_hid, wavey, female_income_share) |>
    dplyr::group_by(cpf_hid) |>
    dplyr::filter(wavey == min(wavey)) |>
    dplyr::distinct() |>
    dplyr::pull(female_income_share)
  
  # Calculate density estimates for the selected data around the cutoff
  rdplot <- rdplotdensity(rddensity(plot_data, c = cutoff), plot_data)
  
  # Compute confidence intervals for the density estimates and structure the output
  rdplot_data <- list(
    rdplotLeft = rdplot$Estl$Estimate,
    rdplotRight = rdplot$Estr$Estimate
  ) |>
    purrr::map(as.data.frame) |>
    purrr::map(~ dplyr::mutate(
      .,
      cil = f_q - qnorm(0.975) * se_q,
      ciu = f_q + qnorm(0.975) * se_q
    ))
  
  return(rdplot_data)
}

# Function to create a plot based on the prepared data and a specified cutoff
# Inputs:
# - rdplotLeft: A dataframe for the left side of the cutoff
# - rdplotRight: A dataframe for the right side of the cutoff
# - cutoff: A numeric value indicating the cutoff point
# Outputs:
# - A ggplot object which can be displayed or saved
create_plot <- function(rdplotLeft, rdplotRight, cutoff) {
  ggplot() +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    geom_ribbon(data = rdplotLeft, aes(x = grid, ymin = cil, ymax = ciu), alpha = 0.2, fill = "gray50") +
    geom_ribbon(data = rdplotRight, aes(x = grid, ymin = cil, ymax = ciu), alpha = 0.2, fill = "gray50") +
    geom_line(data = rdplotLeft, aes(x = grid, y = f_p), col = "black", linewidth = 0.8) +
    geom_line(data = rdplotRight, aes(x = grid, y = f_p), col = "black", linewidth = 0.8) +
    geom_vline(xintercept = cutoff, linetype = "dashed", color = "gray30") +
    # it is clearly not a fraction since it goes above 1
    labs(x = "Share earned by the wife", y = "Fraction of couples") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, max(rdplotLeft$ciu, rdplotRight$ciu)))
}

# Usage example
# Assuming psid_model_data is a dataframe loaded elsewhere containing the required columns
plot_data <- prepare_plot_data(psid_model_data, 0.5)
plot <- create_plot(plot_data$rdplotLeft, plot_data$rdplotRight, 0.5)
print(plot)
