suppressMessages(suppressWarnings(library(dplyr)))

## install packages if not available on host computer
required_packages <- dplyr::tribble(
    ~packages,    ~version,
    "beepr",      "1.3",
    "data.table", "1.12.2",
    "devtools",   "2.1.0",
    "doBy",       "4.6.2",
    "easycsv",    "1.0.8",
    "gridExtra",  "2.3",
    "hms",        "0.5.0",
    "lubridate",  "1.7.4",
    "magrittr",   "1.5",
    "readxl",     "1.3.1",
    "reshape2",   "1.4.3",
    "stringr",    "1.4.0",
    # "tidyverse",  "1.2.1",
    "dplyr",      "1.0.9", 
    "ggplot2",    "3.3.6",
    "tidyr",      "1.2.0",
    "writexl",    "1.2"
)

if(first_run) {
    ## install essential packages first
    essential <- c("dplyr","devtools")
    essential <- essential[!(essential %in% installed.packages()[,"Package"])]
    
    if(length(essential)) {
        cat("\n")
        cat("To start off, R will install the essential packages.\n")
        Sys.sleep(2)
        install.packages(essential)
    }
    
    suppressMessages(suppressWarnings(library(dplyr)))
    suppressMessages(suppressWarnings(library(devtools)))
    
    # devtools::update_version("ggplot2", version = "0.9.1", repos = "http://cran.us.r-project.org"))
    
    new_packages <- 
        required_packages %>% 
        dplyr::filter(!(required_packages$packages %in% installed.packages()[,"Package"])) %>% 
        dplyr::pull(packages)
    
    if(length(new_packages)) {
        cat("\n")
        cat("WARNING: Your computer does not have other required packages.\n")
        cat("R will now install the missing packages. Please wait...\n")
        Sys.sleep(2)
        install.packages(new_packages)
        cat("\n")
        cat("...All required packages to run this script have been installed!\n")
        Sys.sleep(2)
    }
    
    ## check whether required packages are updated
    required_packages <- required_packages %>% dplyr::mutate(version_current = NA)
    
    for(ii in 1:nrow(required_packages)) { 
        required_packages$version_current[ii] <- as.character(packageVersion(required_packages$packages[ii]))
    }
    
    outdated <- 
        required_packages %>% 
        dplyr::mutate(outdated = version_current < version) %>% 
        dplyr::filter(outdated == TRUE) %>% 
        dplyr::pull(packages)
    
    if(length(outdated)) {
        cat("\n")
        cat("WARNING: Your computer has some outdated packages.\n")
        cat("R will now update these packages. Please wait...\n")
        Sys.sleep(2)
        update_packages(outdated)
        cat("\n")
        cat("...All  packages are up to date to run this script!\n")
        Sys.sleep(2)
    }
}

## load required packages
suppressMessages(suppressWarnings( lapply(required_packages$packages, require, character.only = TRUE) ))
# rm(new_packages, outdated, required_packages)
