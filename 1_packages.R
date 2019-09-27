## install packages if not available on host computer
required_packages <- c("beepr", 
                      "data.table", "doBy", 
                      "easycsv",
                      "gridExtra",
                      "hms",
                      "lubridate",
                      "magrittr", 
                      "readxl", "reshape2",
                      "stringr",
                      "tidyverse")

new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if(length(new_packages)) {
    cat("\n")
    cat("WARNING: Your computer does not have the required packages.\n")
    cat("R will now install the missing packages. Please wait...\n")
    Sys.sleep(2)
    install.packages(new_packages)
    cat("\n")
    cat("...All required packages to run this script have been installed!\n")
    Sys.sleep(2)
}

## load required packages
suppressMessages(suppressWarnings( lapply(required_packages, require, character.only = TRUE) ))
rm(new_packages, required_packages)