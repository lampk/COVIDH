data_path <- file.path(Lmisc::get.dropbox.folder(), "Workspace", "Database", "COVID19", "NCSC")

get_ncsc_data(url = "https://covid.ncsc.gov.vn/api/v3/covid/province/", rawdata = file.path(data_path, "raw"), cleandata = file.path(data_path, "clean"),
                          url_vaccine = "https://covid.ncsc.gov.vn/api/v3/vaccine/national_injected_by_day?filter_type=people")
