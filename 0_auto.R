source("1_packages.R")

user_index <- which(unlist(strsplit(getwd(), "/")) == "Users") + 1
user <- unlist(strsplit(getwd(), "/"))[user_index]
filesDir <- paste0("C:\\Users\\", user, "\\Dropbox\\work\\DNF\\help_Vittoria\\sleepingFlies\\flyAway_v4.0\\test_files\\")

readxl::read_excel(paste0(filesDir, "\\flyTable.xlsx")) %>%
    dplyr::mutate(start_date = lubridate::ymd(start_date),
                  start_hour = hms::as.hms(paste(lubridate::hour(start_hour), lubridate::minute(start_hour), lubridate::second(start_hour), sep = ":")),
                  start_time = lubridate::ymd_hms(paste0(start_date, start_hour))) %>%
    dplyr::select(import_folder:start_hour, start_time, experiment_interval:export_folder) -> flyTable
