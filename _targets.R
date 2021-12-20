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

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "Lmisc"))

# End this file with a list of target objects.
list(
  ## update data
  ### NCSC
  tar_target(name = ncsc_data_path,
             command = get_ncsc_data(url = ncsc_url,
                                     url_vaccine = ncsc_url_vaccine,
                                     rawdata = file.path(data_path, "NCSC", "raw"),
                                     cleandata = file.path(data_path, "NCSC", "clean"),
                                     backup = file.path(data_path, "NCSC", "backup")),
             format = "file"),
  ### HCDC BC truc
  tar_target(name = bctruc_data,
             command = get_case_bctruc_data(case_url = case_url,
                                            bctruc_url = bctruc_url,
                                            download = "bctruc",
                                            outdir = file.path(data_path, "HCDC", "daily"),
                                            timestamp = gsub(pattern = "-|:| |+", replacement = "", x = Sys.time())),
             format = "file")
  ### HCDC CDS PCR
  ### HCDC CDS Test nhanh
  ### DOH tom tat
  ### DOH chi tiet - BN
  ### DOH chi tiet - tuvong
)
