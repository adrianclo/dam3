##-------------------------------------------------------------------##
##  WRITTEN BY:   ADRIAN C. LO, PhD                                  ##
##  REQUESTED BY: VITTORIA MARIANO, MSc                              ##
##  PURPOSE:      To read data from Trikinetic System (DAM3)         ##
##                To investigate fly behavioral activity             ##
##                                                                   ##
##  VERSION:      4.4                                                ##
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
cat("Version 4.0: 26-07-2018\n         .1: 21-09-2018\n         .2: 25-09-2018\n         .3: 27-09-2018\n         .4: 02-10-2018\n\n")

cat("PURPOSE: This program will read in your .txt file(s) containing SLEEP and AWAKE activity in flies\n")
cat("         Data are exported to a folder specified in your flyTable.xlsx.\n\n")

rm(list = ls())
source("1_packages.R")

filesDir <- choose_dir() # C:/this/directory
# filesDir = "C:/Users/Alo1/Dropbox/work/DNF/help_Vittoria/sleepingFlies/flyAway_v4.4/test_files_new/"
# filesDir = "C:/Users/adria/Dropbox/work/DNF/help_Vittoria/sleepingFlies/flyAway_v4.4/test_files_new/"
# filesDir = "C:/Users/Adrian/Dropbox/work/DNF/help_Vittoria/sleepingFlies/flyAway_v4.4/test_files_new/"
if(!str_sub(filesDir, -1) == "/") { filesDir <- paste0(filesDir, "/") }

## import flyTable
read_excel(paste0(filesDir, "flyTable.xlsx")) %>%
    mutate(start_date = ymd(start_date),
           # start_time = hms::as.hms(paste(start_hour,start_min,"00", sep = ":"))) %>% # hms::as.hms is deprecated
           start_time = hms::as_hms(paste(start_hour,start_min,"00", sep = ":"))) %>%
    select(-c(start_min,start_hour)) %>%
    mutate(start = ymd_hms(paste(start_date, start_time))) %>%
    select(import_file:start_date, start_time, start, start_zt:experiment_interval, export_folder) -> flyTable

# aa = 1
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
