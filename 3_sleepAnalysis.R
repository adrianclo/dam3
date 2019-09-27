cat("-- analyzing sleep parameters\n")

# overall analysis: minutes sleep -------------------------------------------------------------

## sleep per hour quantified in minutes over each day per fly
sleep %>%
    select(-c(time, zt_demi)) %>%
    group_by(day, phase, zt) %>%
    summarise_all(sum) %>% ungroup() -> sleep_day

sleep_day %>%
    mutate(graph_x = rep(0:23, times = flyTable$experiment_interval[aa])) %>% # extra line
    select(day:zt, graph_x, contains("fly")) %>% # extra line
    gather(fly_id, sleep, -c(day, phase, zt, graph_x)) %>%
    ggplot(aes(graph_x, sleep)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_rect(mapping = aes(xmin = which(zt_sequence == head(night_sequence,1)) -1, 
                            xmax = which(zt_sequence == tail(night_sequence,1)) -1, ymin = 0, ymax = 60), fill = "gray", alpha = 1/5) +
    geom_point(alpha = 1/10) +
    stat_summary(geom = "point", fun.y = mean, color = "blue") +
    stat_summary(geom = "errorbar", fun.data = mean_se, color = "blue", width = .1) +
    stat_summary(geom = "line", fun.y = mean, color = "blue") +
    labs(x = "ZT", y = "Sleep (min/hour)", title = flyTable$import_file[aa]) +
    scale_x_continuous(breaks = seq(0,23,3),
                       labels = zt_sequence[seq(1,length(zt_sequence),3)]) + # extra line
    facet_grid(day ~ ., labeller = label_both)
ggsave(paste0(filesDir, subDir, "sleep_pattern.png"), width = 8.58, height = 3.11)

## sleep: day, night, total
sleep_day %>%
    gather(fly_id, sleep, -c(day, phase, zt)) %>%
    group_by(fly_id, day, phase) %>%
    summarise(sleep_phase = sum(sleep)) %>%
    mutate(phase = as.character(phase)) %>%
    spread(fly_id, sleep_phase) -> sleep_phase_temp
sleep_day %>%
    gather(fly_id, sleep, -c(day, phase, zt)) %>%
    group_by(fly_id, day) %>%
    summarise(sleep_phase = sum(sleep)) %>%
    mutate(phase = "TOTAL") %>%
    select(fly_id, day, phase, sleep_phase) %>%
    spread(fly_id, sleep_phase) %>%
    bind_rows(sleep_phase_temp) %>%
    mutate(phase = factor(phase, levels = c("Night","Day","TOTAL"))) %>%
    arrange(phase, day) -> sleep_phase

# sleep parameters ----------------------------------------------------------------------------

nBouts_day = aveDurationBouts_day = maxDurationBouts_day = maxDurationZT_day = 
    latency_day = ci_day = waso_day = briefAwake_day = briefAwake_koh_day = 
    nBouts_night = aveDurationBouts_night = maxDurationBouts_night = maxDurationZT_night =
    latency_night = ci_night = waso_night = briefAwake_night = briefAwake_koh_night =
    matrix(numeric(length(flies)*flyTable$experiment_interval[aa]), ncol = flyTable$experiment_interval[aa])
colnames(nBouts_day) = colnames(aveDurationBouts_day) = colnames(maxDurationBouts_day) = colnames(maxDurationZT_day) =
    colnames(latency_day) = colnames(ci_day) = colnames(waso_day) = colnames(briefAwake_day) = colnames(briefAwake_koh_day) =
    paste0("Day_", 1:flyTable$experiment_interval[aa])
colnames(nBouts_night) = colnames(aveDurationBouts_night) = colnames(maxDurationBouts_night) = colnames(maxDurationZT_night) =
    colnames(latency_night) = colnames(ci_night) = colnames(waso_night) = colnames(briefAwake_night) = colnames(briefAwake_koh_night) =
    paste0("Night_", 1:flyTable$experiment_interval[aa])
boutLength_df = tibble()

# jj = 1
for(jj in 1:flyTable$experiment_interval[aa]) {
    ## subset per day
    temp_file <- filter(sleep, day == jj)
    # ii = 6
    for(ii in 6:dim(sleep)[2]) {
        ## subset per phase
        zzz_day <- rle(pull(temp_file[temp_file$phase == "Day", ii]))
        tibble(values = zzz_day$values, lengths = zzz_day$lengths) %>%
            mutate(end = cumsum(lengths),
                   start = end - (lengths - 1),
                   zt_start = floor(start / 60)) -> zzz_day
        zzz_night <- rle(pull(temp_file[temp_file$phase == "Night", ii]))
        tibble(values = zzz_night$values, lengths = zzz_night$lengths) %>%
            mutate(end = cumsum(lengths),
                   start = end - (lengths - 1),
                   zt_start = floor(start / 60)) -> zzz_night
        
        ## sleep bout amount
        nBouts_day[ii-5,jj] <- sum(zzz_day$values == TRUE)
        nBouts_night[ii-5,jj] <- sum(zzz_night$values == TRUE)
        
        ## sleep bout lengths
        temp_day <- zzz_day$lengths[zzz_day$values == TRUE]
        boutLength_df %<>% bind_rows(tibble(fly = rep(names(sleep)[ii], length(temp_day)), 
                                                phase = rep("Day", length(temp_day)),
                                                day = rep(jj, length(temp_day)), 
                                                lengths = temp_day))
        temp_night <- zzz_night$lengths[zzz_night$values == TRUE]
        boutLength_df %<>% bind_rows(tibble(fly = rep(names(sleep)[ii], length(temp_night)), 
                                                phase = rep("Night", length(temp_night)),
                                                day = rep(jj, length(temp_night)), 
                                                lengths = temp_night))
        
        ## sleep parameters
        if(sum(zzz_day$values == TRUE) > 0) {
            aveDurationBouts_day[ii-5,jj] <- round(mean(zzz_day$lengths[zzz_day$values == TRUE]), 2)
            maxDurationBouts_day[ii-5,jj] <- max(zzz_day$lengths[zzz_day$values == TRUE])
            maxDurationZT_day[ii-5,jj] <- zzz_day %>% filter(values == TRUE) %>% filter(lengths == max(lengths)) %>% slice(1) %>% pull(zt_start)        ## sleep latency
            if(zzz_day$values[1] == TRUE) { latency_day[ii-5,jj] <- 0 } else { latency_day[ii-5,jj] <- zzz_day$lengths[1] + 1 }
            ## consolidation index
            ci_day[ii-5,jj] <- sum(zzz_day$lengths[zzz_day$values == TRUE]^2) / sum(zzz_day$lengths[zzz_day$values == TRUE])
            ## wake after sleep onset
            if(zzz_day$values[1] == FALSE) { waso_day[ii-5,jj] <- sum(zzz_day$lengths[zzz_day$values == FALSE][-1]) } else { waso_day[ii-5,jj] <- sum(zzz_day$lengths[zzz_day$values == FALSE]) }
            ## brief awakenings default: 1-min awake epoch between two sleep sequences
            briefAwake_day[ii-5,jj] = sum(zzz_day$lengths[zzz_day$values == FALSE] == 1) 
            ## brief awakenings following Koh K, Evans JM, Hendricks JC, Sehgal A (2006). PMID: 16938867
            briefAwake_koh_day[ii-5,jj] = sum(zzz_day$lengths[zzz_day$values == FALSE] <= 4)
        } else {
            aveDurationBouts_day[ii-5,jj] <- 0
            maxDurationBouts_day[ii-5,jj] <- 0
            maxDurationZT_day[ii-5,jj] <- NA
            latency_day[ii-5,jj] <- 60 * (24 - flyTable$night_duration[aa])
            ci_day[ii-5,jj] <- 0
            waso_day[ii-5,jj] <- NA # or max score?
            briefAwake_day[ii-5,jj] <- NA
            briefAwake_koh_day[ii-5,jj] <- NA
        }
        if(sum(zzz_night$values == TRUE) > 0) {
            aveDurationBouts_night[ii-5,jj] <- round(mean(zzz_night$lengths[zzz_night$values == TRUE]), 2)
            maxDurationBouts_night[ii-5,jj] <- max(zzz_night$lengths[zzz_night$values == TRUE])
            maxDurationZT_night[ii-5,jj] <- zzz_night %>% filter(values == TRUE) %>% filter(lengths == max(lengths)) %>% slice(1) %>%  pull(zt_start)
            if(zzz_night$values[1] == TRUE) { latency_night[ii-5,jj] <- 0 } else { latency_night[ii-5,jj] <- zzz_night$lengths[1] + 1 }
            ci_night[ii-5,jj] <- sum(zzz_night$lengths[zzz_night$values == TRUE]^2) / sum(zzz_night$lengths[zzz_night$values == TRUE])
            if(zzz_night$values[1] == FALSE) { waso_night[ii-5,jj] <- sum(zzz_night$lengths[zzz_night$values == FALSE][-1]) } else { waso_night[ii-5,jj] <- sum(zzz_night$lengths[zzz_night$values == FALSE]) }
            briefAwake_night[ii-5,jj] = sum(zzz_night$lengths[zzz_night$values == FALSE] == 1) 
            briefAwake_koh_night[ii-5,jj] = sum(zzz_night$lengths[zzz_night$values == FALSE] <= 4)
        } else {
            aveDurationBouts_night[ii-5,jj] <- 0
            maxDurationBouts_night[ii-5,jj] <- 0
            maxDurationZT_night[ii-5,jj] <- NA
            latency_night[ii-5,jj] <- 60 * flyTable$night_duration[aa]
            ci_night[ii-5,jj] <- 0
            waso_night[ii-5,jj] <- NA
            briefAwake_night[ii-5,jj] <- NA
            briefAwake_koh_night[ii-5,jj] <- NA
        }
    }
}

if(tolower(flyTable$start_phase[1]) == "day") {
    nBouts <- bind_cols(fly_id = flies, tbl_df(nBouts_day), tbl_df(nBouts_night))
    aveDurationBouts <- bind_cols(fly_id = flies, tbl_df(aveDurationBouts_day), tbl_df(aveDurationBouts_night))
    maxDurationBouts <- bind_cols(fly_id = flies, tbl_df(maxDurationBouts_day), tbl_df(maxDurationBouts_night))
    maxDurationZT <- bind_cols(fly_id = flies, tbl_df(maxDurationZT_day), tbl_df(maxDurationZT_night))
    latency <- bind_cols(fly_id = flies, tbl_df(latency_day), tbl_df(latency_night))
    ci <- bind_cols(fly_id = flies, tbl_df(ci_day), tbl_df(ci_night))
    waso <- bind_cols(fly_id = flies, tbl_df(waso_day), tbl_df(waso_night))
    briefAwake <- bind_cols(fly_id = flies, tbl_df(briefAwake_day), tbl_df(briefAwake_night))
    briefAwake_koh <- bind_cols(fly_id = flies, tbl_df(briefAwake_koh_day), tbl_df(briefAwake_koh_night))
    boutLength_df %<>% arrange(phase, day) 
} else {
    nBouts <- bind_cols(fly_id = flies, tbl_df(nBouts_night), tbl_df(nBouts_day))
    aveDurationBouts <- bind_cols(fly_id = flies, tbl_df(aveDurationBouts_night), tbl_df(aveDurationBouts_day))
    maxDurationBouts <- bind_cols(fly_id = flies, tbl_df(maxDurationBouts_night), tbl_df(maxDurationBouts_day))
    maxDurationZT <- bind_cols(fly_id = flies, tbl_df(maxDurationZT_night), tbl_df(maxDurationZT_day))
    latency <- bind_cols(fly_id = flies, tbl_df(latency_night), tbl_df(latency_day))
    ci <- bind_cols(fly_id = flies, tbl_df(ci_night), tbl_df(ci_day))
    waso <- bind_cols(fly_id = flies, tbl_df(waso_night), tbl_df(waso_day))
    briefAwake <- bind_cols(fly_id = flies, tbl_df(briefAwake_night), tbl_df(briefAwake_day))
    briefAwake_koh <- bind_cols(fly_id = flies, tbl_df(briefAwake_koh_night), tbl_df(briefAwake_koh_day))
    boutLength_df %<>% arrange(desc(phase), day)
}

hist_day <- ggplot(filter(boutLength_df, phase == "Day"), aes(lengths)) +
    geom_histogram(binwidth = 5) +
    geom_vline(xintercept = median(filter(boutLength_df, phase == "Day")$lengths), color = "navyblue") +
    annotate("text", x = median(filter(boutLength_df, phase == "Day")$lengths), y = 0, vjust = 0, label = median(filter(boutLength_df, phase == "Day")$lengths)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) +
    labs(title = paste(flyTable$import_file[aa], ":: Bout length distribution during day phase"))
hist_night <- ggplot(filter(boutLength_df, phase == "Night"), aes(lengths)) +
    geom_histogram(binwidth = 5) +
    geom_vline(xintercept = median(filter(boutLength_df, phase == "Night")$lengths), color = "navyblue") +
    annotate("text", x = median(filter(boutLength_df, phase == "Night")$lengths), y = 0, vjust = 0, label = median(filter(boutLength_df, phase == "Night")$lengths)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) +
    labs(title = paste(flyTable$import_file[aa], ":: Bout length distribution during night phase"))
g_hist <- grid.arrange(hist_day, hist_night, nrow = 2)
ggsave(filename = paste0(filesDir, subDir, "sleep_bout_length.png"), g_hist, width = 8.58, height = 4.68)

# export --------------------------------------------------------

write.table(sleep_day, paste0(filesDir, subDir, "sleep_pattern.txt"), quote = F, sep = "\t", row.names = F)
write.table(sleep_phase, paste0(filesDir, subDir, "sleep_phase.txt"), quote = F, sep = "\t", row.names = F)
write.table(nBouts, paste0(filesDir, subDir, "sleep_bout_n.txt"), sep = "\t", quote = F, row.names = F)
write.table(aveDurationBouts, paste0(filesDir, subDir, "sleep_bout_aveDur.txt"), sep = "\t", quote = F, row.names = F)
write.table(maxDurationBouts, paste0(filesDir, subDir, "sleep_bout_maxDur.txt"), sep = "\t", quote = F, row.names = F)
write.table(maxDurationZT, paste0(filesDir, subDir, "sleep_bout_maxDur_ZT.txt"), sep = "\t", quote = F, row.names = F)
write.table(latency, paste0(filesDir, subDir, "sleep_latency.txt"), sep = "\t", quote = F, row.names = F)
write.table(ci, paste0(filesDir, subDir, "sleep_ciIndex.txt"), sep = "\t", quote = F, row.names = F)
write.table(waso, paste0(filesDir, subDir, "sleep_waso.txt"), sep = "\t", quote = F, row.names = F)
write.table(briefAwake, paste0(filesDir, subDir, "sleep_briefAwake.txt"), sep = "\t", quote = F, row.names = F)
write.table(briefAwake_koh, paste0(filesDir, subDir, "sleep_briefAwake_koh.txt"), sep = "\t", quote = F, row.names = F)
write.table(boutLength_df, paste0(filesDir, subDir, "sleep_bout_length.txt"), sep = "\t", quote = F, row.names = F)

rm(list = setdiff(ls(), c("sleep", "activity", "flyTable",
                          "filesDir", "subDir", "aa",
                          "zt_sequence", "day_sequence", "night_sequence",
                          "flies")))
