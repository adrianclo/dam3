cat("-- dataformatting\n")

# activity data.frame -------------------------------------------------------------------------

## add flexibility in assigning start zt, and which zts are day or night
#### zt_sequence
zt_sequence <- seq(flyTable$start_zt[aa], length.out = 24)
if(sum(zt_sequence > 23)) { zt_sequence[zt_sequence > 23] <- zt_sequence[zt_sequence > 23] - 24 }

#### zt_demi_sequence
if(zt_sequence[1] != 0) { 
    zt_1 <- zt_sequence[1:which.max(zt_sequence)]; zt_1 <- seq(head(zt_1,1), tail(zt_1,1) + .5, by = .5)
    zt_2 <- setdiff(zt_sequence, zt_1); zt_2 <- seq(head(zt_2,1), tail(zt_2,1) + .5, by = .5)
    zt_demi_sequence <- c(zt_1,zt_2); rm(zt_1,zt_2)
} else { zt_demi_sequence <- seq(zt_sequence[1], zt_sequence[24] + .5, by = .5) }

#### phase
if(tolower(flyTable$start_phase[aa]) == "day") {
    night_sequence <- seq(flyTable$start_zt[aa] + flyTable$phase_shift[aa], length.out = flyTable$night_duration[aa])
    ## correct for numbers larger than 23
    if(sum(night_sequence > 23)) { night_sequence[night_sequence > 23] <- night_sequence[night_sequence > 23] - 24 }
    day_sequence <- setdiff(0:23,
                            night_sequence)
} else {
    day_sequence <- seq(flyTable$start_zt[aa] + flyTable$phase_shift[aa], length.out = 24 - flyTable$night_duration[aa])
    if(sum(day_sequence > 23)) { day_sequence[day_sequence > 23] <- day_sequence[day_sequence > 23] - 24 }
    night_sequence <- setdiff(0:23,
                              day_sequence) }

## import data and filter requested time window
activity <- fread(paste0(filesDir, flyTable$import_file[aa]), header = FALSE, stringsAsFactors = FALSE)
# activity %<>% 
    activity <- activity %>%
    mutate(time = ymd_hms(paste(dmy(activity$V2), hms::as_hms(activity$V3)))) %>%
    filter(between(time, # subset to data of interest
                   flyTable$start[aa],
                   (flyTable$start[aa]-60) + days(flyTable$experiment_interval[aa])))

## remove rows with code 51: that is when the sleep deprivation apparatus is switched on, and produces a double entry per time point
activity <- activity %>% filter(V4 != 51)

## check whether requested time window is available in .txt file
if(head(activity$time,1) != flyTable$start[aa]) {
    stop("Time window issue: requested startpoint not available in .txt file")
}

if(tail(activity$time,1) != 
   ((flyTable$start[aa]-60) + days(flyTable$experiment_interval[aa]))) {
    stop("Time window issue: requested endpoint not available in .txt file")
}

## start dataformatting
# activity %<>%
    activity <- activity %>%
    mutate(day = rep(1:flyTable$experiment_interval[aa], each = 60 * 24), # add date-independent parameters
           zt = rep(zt_sequence, each = 60, times = flyTable$experiment_interval[aa]),
           zt_demi = rep(zt_demi_sequence, each = 30, times = flyTable$experiment_interval[aa]),
           phase = factor(case_when(zt %in% day_sequence ~ "Day",
                             zt %in% night_sequence ~ "Night"),
                          levels = c("Day","Night"))) %>%
    select(time, day, zt, zt_demi, phase, V11:V42)
id <- as.character(1:32); id[nchar(id) == 1] <- paste0("0", id[nchar(id) == 1])
names(activity)[6:37] = paste("fly", id, sep = "_")

if(activity$phase[1] == "Night") { activity$phase <- factor(activity$phase, levels = c("Night","Day")) }

## control code
# activity %>%
#     group_by(zt) %>%
#     summarise(phase = first(phase)) %>%
#     as.data.frame()

# activity %>% gather(fly_id, activity, -c(time:phase))

activity <- as_tibble(activity)
ww <- which(apply(activity[,6:37], 2, sum) == 0) # check for 100% inactive/dead flies
if(length(ww) > 0) { activity = activity[,-(ww + 5)] } # remove 100% inactive/dead flies
flies <- names(activity)[6:ncol(activity)] # update number of flies

# assemble sleep data.frame -------------------------------------------------------------------

## TRUE for >= 5-length series of 0 (proxy for inactivity/sleep)
# activity %>%
#     select(contains("fly")) %>%
#     mutate_all(funs(if_else(. == 0, TRUE, FALSE))) -> sleep # funs is soft deprecated. funs(name = f(.)) is now list(name = ~f(.))
sleep <- activity %>%
    select(contains("fly")) %>%
    mutate_all(list(~if_else(. == 0, TRUE, FALSE)))
# ii = 1
for(ii in 1:dim(sleep)[2]) {
    identifier = rle(as.matrix(sleep)[,ii]); identifier = tibble(values = identifier$values, lengths = identifier$lengths)
    # identifier %<>%
    identifier <- identifier %>% 
        mutate(values = case_when(values == TRUE & lengths < 5 ~ FALSE,
                                  TRUE ~ as.logical(values)))
    sleep[,ii] = rep(identifier$values, times = identifier$lengths) }

sleep <- bind_cols(activity[,1:5], sleep)

# export --------------------------------------------------------

write.table(activity, paste0(filesDir, subDir, "activity_raw.txt"), quote = F, sep = "\t", row.names = F)
write.table(sleep, paste0(filesDir, subDir, "sleep_raw.txt"), quote = F, sep = "\t", row.names = F)
rm(identifier)
