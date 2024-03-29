##-------------------------------------------------------------------##
##  WRITTEN BY:   ADRIAN C. LO, PhD                                  ##
##  REQUESTED BY: VITTORIA MARIANO, MSc                              ##
##  PURPOSE:      To read data from Trikinetic System (DAM3)         ##
##                To investigate fly behavioral activity             ##
##                                                                   ##
##  VERSION:      4.6                                                ##
##                "MASS-IMPORTER"                                    ##
##-------------------------------------------------------------------##

cat("\014")
cat("======================================\n")
cat("TRIKINETIC SYSTEM DATA ANALYZER (DAM3)\n")
cat("======================================\n")
cat("WRITTEN BY: ADRIAN C. LO, PhD\n")
cat("Version 1.0: 09-06-2016\n         .1: 15-06-2016\n")
cat("Version 2.0: 01-12-2016\n         .1: 28-05-2017\n         .2: 12-06-2017\n")
cat("Version 3.0: 22-08-2017\n         .1: 10-05-2018\n         .2: 29-05-2018\n")
cat("Version 4.0: 26-07-2018\n         .1: 21-09-2018\n         .2: 25-09-2018\n         .3: 27-09-2018\n         .4: 02-10-2018\n")
cat("         .5: 03-02-2020\n         .6: 12-03-2023\n\n")

# update version 4.6:
# - have explicit library references in functions

cat("PURPOSE: This program will read in your .txt file(s) containing SLEEP and AWAKE activity in flies\n")
cat("         Data are exported to a folder specified in your flyTable.xlsx.\n\n")

first_run <- F
source("1_packages.R")

filesDir <- easy_csv::choose_dir() # C:/this/directory
if(!stringr::str_sub(filesDir, -1) == "/") { filesDir <- paste0(filesDir, "/") }

## import flyTable
flyTable <-
    readxl::read_excel(paste0(filesDir, "flyTable.xlsx")) %>%
    dplyr::mutate(start_date = lubridate::ymd(start_date),
                  # start_time = hms::as.hms(paste(start_hour,start_min,"00", sep = ":"))) %>% # hms::as.hms is deprecated
                  start_time = hms::as_hms(paste(start_hour,start_min,"00", sep = ":"))) %>%
    dplyr::select(-c(start_min, start_hour)) %>%
    dplyr::mutate(start = lubridate::ymd_hms(paste(start_date, start_time))) %>%
    dplyr::select(import_file:start_date, start_time, start, start_zt:experiment_interval, 
                  subset_start_zt, subset_end_zt,
                  export_folder)

# aa <- 1
for(aa in 1:nrow(flyTable)) {
    cat("Processing file", aa, "::", flyTable$import_file[aa], "\n")
    ## Create subdirectory to export files to
    subDir <- paste0(flyTable$export_folder[aa], "/")
    
    if(!file.exists(subDir)) dir.create(file.path(filesDir, subDir))
    
    source("2_dataformat.R")
    source("3_sleepAnalysis.R")
    source("4_activityAnalysis.R")
    cat("-- exporting:", flyTable$export_folder[aa], "\n")
}

cat("\nAll files processed!\n"); beep(1)
