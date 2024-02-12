# This function checks if packages are installed, installed them if not, and
# loads thems. Packages are named inside a vector of strings.

load_packages <- function(packages = c("dplyr")) {
  is_installed <- function(pack) {
    # checks if package is installed
    test <- length(nzchar(find.package(package = pack, quiet = TRUE)))

    return(test == 1)
  }

  # vector with uninstalled packages
  uninstalled <- packages[!c(sapply(packages, is_installed))]

  # install uninstalled packages and load all dependencies
  install.packages(uninstalled)
  invisible(lapply(packages, library, character.only = TRUE))
}

# Example usage:
# required_packages <- c(
#   "arrow",
#   "duckdb",
#   "dplyr",
#   "fixest",
#   "stringr",
#   "purrr",
#   "broom",
#   "tidyr",
#   "gt"
# )

# load_packages(required_packages)