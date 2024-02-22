# Function to read PSID data
read_psid_data <- function(filepath) {
  read_feather(
    filepath,
    col_select = c(
      cpf_pid, # person UUID
      cpf_hid, # household UUID
      wavey, # wave year
      rel,
      female, # female dummy
      age,
      edu4,
      mlstat5, # marital status
      livpart,
      kidsn_hh17, # kids under 17
      emplst6,
      incjob1_mg,
      hhinc_post,
      hisp,
      hwork,
      rstate
    )
  )
}

# Example usage:
# psid <- read_psid_data("data/raw/psid_hufe.arrow")
