#' Program written by: Adrian Lo
#' REQUEST FROM: VITTORIA MARIANO
#' VERSION: 1.1

#' OBJECTIVE of program:
#' 1. read in txt file
#' 2. per column of original file:
#'      a. identify series of >= 5 times 0's
#'      b. number of such series
#'      c. when these series occurred (+ duration)

#' V1.1 adjustments:
#' - included date info
#' - changed order of package installment and explanation provided

sleepingflies = function() {
    # presets and explanations --------------------------------------------------------------------
    
    ## install packages if not available on host computer
    required.packages = c("dplyr", "ggplot2")
    new.packages = required.packages[!(required.packages %in% installed.packages()[, "Package"])]
    if(length(new.packages)) {
        cat("Your computer does not have the required packages.")
        cat("\nR will now install the required packages. Please wait")
        Sys.sleep(5)
        install.packages(new.packages)
    }
    suppressMessages(suppressWarnings(library(dplyr)))
    
    ## provide explanation on the purpose of the program
    cat("Program was written by Adrian Lo. \nDate: 2016-06-09")
    cat("\nLast revision: 2017-03-06")
    
    cat("\n\nThis program will read in your \"sleepingflies\" txt file")
    cat("\nand identify whether and when flies have been sleeping.")
    
    cat("\n\nSleeping is defined as a pattern of >= 5 zeros (i.e. minutes).")
    
    cat("\n\nThe program exports its output in a folder called \"sleepingflies\".")
    
    cat("\n\nsleepingflies.txt: boolean values indicate WHETHER sleeping has occurred or not.")
    cat("\nsleeping_sessions.txt: shows the NUMBER of sleeping sessions during the course of the experiment.")
    cat("\nfly_xx.txt: shows for each individual fly WHEN sleeping occurred as well as the sleep duration.")

    ## Create subdirectory to export files to
    ## loop-hole for testing purpose
    test = readline(prompt = "Is this Adrian testing the script? (y or n): ")
    if(tolower(test) == "y") {
        mainDir = "C:/Users/Alo1/Dropbox/R files/sleepingFlies_files"
    } else mainDir = getwd()
    
    subDir  = "sleepingflies"
    
    # if(!file.exists(subDir)) dir.create(file.path(mainDir, subDir))
    dir.create(file.path(mainDir, subDir))
    
    cat("\n\nThe program will now start!")
    cat("\n\n\nPlease select your file:")
    
    date = Sys.Date(); ## format(date, format = "%d-%b-%y")
    
    # import file and remove redundant columns ----------------------------------------------------
    
    if(tolower(test) == "y") {
        temp = read.table(paste0(mainDir, "/test_flies.txt"))
    } else temp = read.table(file.choose(), header = FALSE)
    cat("\n\nIN PROGRESS...")
    
    dat = data.frame(date = paste(temp$V2, temp$V3, temp$V4, sep = "-"),
                     hh_mm_ss = temp$V5)
    head(temp, 10)
    head(dat, 10)
    dat2 = as.data.frame(cbind(dat, temp[, 13:(dim(temp)[2])]))
    flies = dim(temp)[2] - 12; rm(temp); flies
    names(dat2)[3:dim(dat2)[2]] = paste("fly", 1:flies, sep = "_")
    head(dat2)
    
    # create dataframe with TRUE-FALSE statements where >= 5 * 0 series are -----------------------
    
    on_off = as.data.frame(matrix(numeric(dim(dat2)[1] * dim(dat2)[2] - dim(dat2)[1] * 2), ncol = flies))
    names(on_off) = paste("fly", 1:flies, sep = "_")
    
    for(ii in 3:dim(dat2)[2]) {
        ## column extraction
        test = dat2[,ii]
        head(test, 10)
        ## identify 0s
        zerosCol = numeric(dim(dat2)[1])        
        zerosCol = rep(FALSE, dim(dat2)[1])
        
        ww = which(test == 0)
        zerosCol[ww] = TRUE
        head(zerosCol, 10)
        
        ## identify series of >= 5
        tmp = rle(test)
        seriesCol = rep(tmp$lengths >= 5, times = tmp$lengths)
        head(seriesCol, 10)
        
        ## matches?
        on_off[,ii-2] = zerosCol & seriesCol
    }
    
    dat = as.data.frame(cbind(dat, on_off)); rm(on_off)
    head(dat2[,1:14])
    head(dat[,1:14])
    
    ggplot(dat2, aes(paste(date, hh_mm_ss), rowMeans(dat2[,3:ncol(dat2)]))) + 
        geom_point(alpha = 1/4) +
        labs(x = "Time", y = "Beam crossings") 
    
    fileInfo = paste0(date, "_sleepingflies.txt")
    write.table(dat, paste0("./sleepingflies/", fileInfo), quote = FALSE, sep = "\t", row.names = FALSE)
    
    # how many sleeping sessions? -----------------------------------------------------------------
    
    sleep_sessions = numeric()
    sleep_sessions_duration = numeric()
    for(ii in 3:dim(dat)[2]) {
        zzz = rle(dat[,ii])
        sleep_sessions[ii-2] = sum(zzz$values == TRUE)
        sleep_sessions_duration[ii-2] = round(mean(zzz$lengths[zzz$values == TRUE]), 2)
    }
    sleep_sessions = data.frame(flyID = paste("fly", 1:flies, sep = "_"),
                                number_session = sleep_sessions, ave_duration = sleep_sessions_duration)
    sleep_sessions
    
    sessionInfo = paste0(date, "_sleep_sessions.txt")
    write.table(sleep_sessions, paste0("./sleepingflies/", sessionInfo), quote = FALSE, sep = "\t", row.names = FALSE)
    
    # when were these sleep sessions? -------------------------------------------------------------
    
    for(ii in 3:dim(dat)[2]) {
        
        lengths = rle(dat[,ii])$lengths
        values = rle(dat[,ii])$values
        
        when = data.frame(lengths = lengths, values = values) %>%
            mutate(rowValue = cumsum(lengths) - (lengths - 1))
        head(when)
        ww = when$rowValue
        
        when = mutate(when, 
                      date = dat$date[ww],
                      hour = dat$hour[ww])
        head(when)
        when = when %>% 
            filter(values == TRUE) %>% 
            select(c(rowValue, date, hour, lengths))
        head(when)
        
        flyInfo = paste0(date, "_fly_")
        write.table(when, paste0("./sleepingflies/", flyInfo, ii-2, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE)
    }
    cat("\n\n\nDONE!")
}

sleepingflies()