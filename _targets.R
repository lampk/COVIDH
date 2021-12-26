library(targets)
library(tarchetypes)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/functions.R")
source(file.path(Lmisc::get.dropbox.folder(), "Workspace", "Database", "COVID19", "paths.R"), local = TRUE)

## update data
today_timestamp <- gsub(pattern = "-|:| |+", replacement = "", x = Sys.time())
### NCSC
get_ncsc_data(url = ncsc_url,
              url_vaccine = ncsc_url_vaccine,
              rawdata = file.path(data_path, "NCSC", "raw"),
              cleandata = file.path(data_path, "NCSC", "clean"),
              backup = file.path(data_path, "NCSC", "backup"))

### HCDC BC truc
get_case_bctruc_data(case_url = case_url,
                     bctruc_url = bctruc_url,
                     download = "bctruc",
                     outdir = file.path(data_path, "HCDC", "daily"),
                     timestamp = today_timestamp)

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "Lmisc"))

# End this file with a list of target objects.
list(
  ## set data
  ### NCSC
  tar_target(name = ncsc_covid,
             command = file.path(data_path, "NCSC", "clean", "covid.rds"),
             format = "file"),
  ### HCDC BC truc
  tar_target(name = bctruc_data,
             command = file.path(data_path, "HCDC", "daily", "bctruc", "combine", paste0("HCDCdata_BCTRUC_", today_timestamp, ".rds")),
             format = "file")
  ### HCDC CDS PCR
  ### HCDC CDS Test nhanh
  ### DOH tom tat
  ### DOH chi tiet - BN
  ### DOH chi tiet - tuvong
)
