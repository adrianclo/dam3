cat("-- analyzing sleep parameters\n")

# overall analysis: minutes sleep -------------------------------------------------------------

## sleep per hour quantified in minutes over each day per fly
sleep_day <-
    sleep %>%
    dplyr::select(-c(time, zt_demi)) %>%
    dplyr::group_by(day, phase, zt) %>%
    dplyr::summarise_all(sum) %>% dplyr::ungroup()

sleep_day_demi <-
    sleep %>%
    dplyr::select(-c(time, zt)) %>%
    dplyr::group_by(day, phase, zt_demi) %>%
    dplyr::summarise_all(sum) %>% dplyr::ungroup()

sleep_day %>%
    dplyr::mutate(graph_x = rep(0:23, times = flyTable$experiment_interval[aa])) %>%
    dplyr::select(day:zt, graph_x, dplyr::contains("fly")) %>%
    tidyr::pivot_longer(names_to = "fly_id",
                        values_to = "sleep", -c(day, phase, zt, graph_x)) %>% # gather(fly_id, sleep, -c(day, phase, zt, graph_x)) %>%
    ggplot2::ggplot(aes(graph_x, sleep)) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::geom_rect(mapping = aes(xmin = which(zt_sequence == head(night_sequence,1)) -1, 
                                     xmax = which(zt_sequence == tail(night_sequence,1)) -1, ymin = 0, ymax = 60), fill = "gray", alpha = 1/5) +
    ggplot2::geom_point(alpha = 1/10) +
    ggplot2::stat_summary(geom = "point", fun = mean, color = "blue") +
    ggplot2::stat_summary(geom = "errorbar", fun.data = mean_se, color = "blue", width = .1) +
    ggplot2::stat_summary(geom = "line", fun = mean, color = "blue") +
    ggplot2::labs(x = "ZT", y = "Sleep (min/hour)", title = flyTable$import_file[aa]) +
    ggplot2::scale_x_continuous(breaks = seq(0,23,3),
                                labels = zt_sequence[seq(1,length(zt_sequence),3)]) +
    ggplot2::facet_grid(day ~ ., labeller = label_both)
ggplot2::ggsave(paste0(filesDir, subDir, "sleep_pattern.png"), width = 8.58, height = 3.11)

sleep_day_demi %>%
    dplyr::mutate(graph_x = rep(seq(0,23.5,.5), times = flyTable$experiment_interval[aa])) %>%
    dplyr::select(day:zt_demi, graph_x, dplyr::contains("fly")) %>%
    tidyr::pivot_longer(names_to = "fly_id",
                        values_to = "sleep", -c(day, phase, zt_demi, graph_x)) %>% # gather(fly_id, sleep, -c(day, phase, zt, graph_x)) %>%
    ggplot2::ggplot(aes(graph_x, sleep)) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::geom_rect(mapping = aes(xmin = which(zt_sequence == head(night_sequence,1)) -1, 
                                     xmax = which(zt_sequence == tail(night_sequence,1)) -1, ymin = 0, ymax = 60), fill = "gray", alpha = 1/5) +
    ggplot2::geom_point(alpha = 1/10) +
    ggplot2::stat_summary(geom = "point", fun = mean, color = "blue") +
    ggplot2::stat_summary(geom = "errorbar", fun.data = mean_se, color = "blue", width = .1) +
    ggplot2::stat_summary(geom = "line", fun = mean, color = "blue") +
    ggplot2::labs(x = "ZT", y = "Sleep (min/30 min)", title = flyTable$import_file[aa]) +
    ggplot2::scale_x_continuous(breaks = seq(0,23,3),
                                labels = zt_sequence[seq(1,length(zt_sequence),3)]) +
    ggplot2::facet_grid(day ~ ., labeller = label_both)
ggplot2::ggsave(paste0(filesDir, subDir, "sleep_pattern (30 min).png"), width = 8.58, height = 3.11)

## sleep: day, night, total
sleep_phase_temp <- 
    sleep_day %>%
    tidyr::pivot_longer(names_to = "fly_id",
                        values_to = "sleep", -c(day, phase, zt)) %>% # gather(fly_id, sleep, -c(day, phase, zt)) %>%
    dplyr::group_by(fly_id, day, phase) %>%
    dplyr::summarise(sleep_phase = sum(sleep), .groups = "drop") %>%
    dplyr::mutate(phase = as.character(phase)) %>%
    tidyr::spread(fly_id, sleep_phase)
sleep_phase <-
    sleep_day %>%
    tidyr::pivot_longer(names_to = "fly_id",
                        values_to = "sleep", -c(day, phase, zt)) %>% # gather(fly_id, sleep, -c(day, phase, zt)) %>%
    dplyr::group_by(fly_id, day) %>%
    dplyr::summarise(sleep_phase = sum(sleep), .groups = "drop") %>%
    dplyr::mutate(phase = "TOTAL") %>%
    dplyr::select(fly_id, day, phase, sleep_phase) %>%
    tidyr::spread(fly_id, sleep_phase) %>%
    dplyr::bind_rows(sleep_phase_temp) %>%
    dplyr::mutate(phase = factor(phase, levels = c("Night","Day","TOTAL"))) %>%
    dplyr::arrange(phase, day)

# sleep parameters ----------------------------------------------------------------------------

# day df
nBouts_day = aveDurationBouts_day = maxDurationBouts_day = maxDurationZT_day = 
    latency_day = ci_day = waso_day = briefAwake_day = briefAwake_koh_day = 
    # night df
    nBouts_night = aveDurationBouts_night = maxDurationBouts_night = maxDurationZT_night =
    latency_night = ci_night = waso_night = briefAwake_night = briefAwake_koh_night =
    # subset df
    ss_nBouts = ss_aveDurationBouts = ss_maxDurationBouts = ss_maxDurationZT = 
    ss_latency = ss_ci = ss_waso = ss_briefAwake = ss_briefAwake_koh = 
    matrix(numeric(length(flies)*flyTable$experiment_interval[aa]), ncol = flyTable$experiment_interval[aa])
colnames(nBouts_day) = colnames(aveDurationBouts_day) = colnames(maxDurationBouts_day) = colnames(maxDurationZT_day) =
    colnames(latency_day) = colnames(ci_day) = colnames(waso_day) = colnames(briefAwake_day) = colnames(briefAwake_koh_day) =
    paste0("Day_", 1:flyTable$experiment_interval[aa])
colnames(nBouts_night) = colnames(aveDurationBouts_night) = colnames(maxDurationBouts_night) = colnames(maxDurationZT_night) =
    colnames(latency_night) = colnames(ci_night) = colnames(waso_night) = colnames(briefAwake_night) = colnames(briefAwake_koh_night) =
    paste0("Night_", 1:flyTable$experiment_interval[aa])
colnames(ss_nBouts) = colnames(ss_aveDurationBouts) = colnames(ss_maxDurationBouts) = colnames(ss_maxDurationZT) = 
    colnames(ss_latency) = colnames(ss_ci) = colnames(ss_waso) = colnames(ss_briefAwake) = colnames(ss_briefAwake_koh) =
    paste0("Subset_", 1:flyTable$experiment_interval[aa])
boutLength_df <- dplyr::tibble()
ss_boutLength_df <- dplyr::tibble()

## overall
for(jj in 1:flyTable$experiment_interval[aa]) {
    ## subset per day
    temp_file <- dplyr::filter(sleep, day == jj)
    
    for(ii in 6:dim(sleep)[2]) {
        ## subset per phase
        zzz_day <- rle(dplyr::pull(temp_file[temp_file$phase == "Day", ii]))
        zzz_day <- dplyr::tibble(values = zzz_day$values, lengths = zzz_day$lengths) %>%
            dplyr::mutate(end = cumsum(lengths),
                          start = end - (lengths - 1),
                          zt_start = floor(start / 60))
        zzz_night <- rle(dplyr::pull(temp_file[temp_file$phase == "Night", ii]))
        zzz_night <- dplyr::tibble(values = zzz_night$values, lengths = zzz_night$lengths) %>%
            dplyr::mutate(end = cumsum(lengths),
                          start = end - (lengths - 1),
                          zt_start = floor(start / 60))
        
        ## sleep bout amount
        nBouts_day[ii-5,jj] <- sum(zzz_day$values == TRUE)
        nBouts_night[ii-5,jj] <- sum(zzz_night$values == TRUE)
        
        ## sleep bout lengths
        temp_day <- zzz_day$lengths[zzz_day$values == TRUE]
        boutLength_df %<>% dplyr::bind_rows(dplyr::tibble(fly = rep(names(sleep)[ii], length(temp_day)), 
                                                          phase = rep("Day", length(temp_day)),
                                                          day = rep(jj, length(temp_day)), 
                                                          lengths = temp_day))
        temp_night <- zzz_night$lengths[zzz_night$values == TRUE]
        boutLength_df %<>% dplyr::bind_rows(dplyr::tibble(fly = rep(names(sleep)[ii], length(temp_night)), 
                                                          phase = rep("Night", length(temp_night)),
                                                          day = rep(jj, length(temp_night)), 
                                                          lengths = temp_night))
        
        ## sleep parameters
        if(sum(zzz_day$values == TRUE) > 0) {
            aveDurationBouts_day[ii-5,jj] <- round(mean(zzz_day$lengths[zzz_day$values == TRUE]), 2)
            maxDurationBouts_day[ii-5,jj] <- max(zzz_day$lengths[zzz_day$values == TRUE])
            maxDurationZT_day[ii-5,jj] <- zzz_day %>% 
                dplyr::filter(values == TRUE) %>% dplyr::filter(lengths == max(lengths)) %>% 
                dplyr::slice(1) %>% dplyr::pull(zt_start)
            ## sleep latency
            if(zzz_day$values[1] == TRUE) { latency_day[ii-5,jj] <- 0 } else { latency_day[ii-5,jj] <- zzz_day$lengths[1] + 1 }
            ## consolidation index
            ci_day[ii-5,jj] <- sum(zzz_day$lengths[zzz_day$values == TRUE]^2) / sum(zzz_day$lengths[zzz_day$values == TRUE])
            ## wake after sleep onset: total awake amount after first sleep bout during the entire phase
            if(zzz_day$values[1] == FALSE) { 
                waso_day[ii-5,jj] <- sum(zzz_day$lengths[zzz_day$values == FALSE][-1]) 
            } else { waso_day[ii-5,jj] <- sum(zzz_day$lengths[zzz_day$values == FALSE]) }
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
            maxDurationZT_night[ii-5,jj] <- zzz_night %>% 
                dplyr::filter(values == TRUE) %>% dplyr::filter(lengths == max(lengths)) %>% 
                dplyr::slice(1) %>%  dplyr::pull(zt_start)
            if(zzz_night$values[1] == TRUE) { 
                latency_night[ii-5,jj] <- 0 
            } else { latency_night[ii-5,jj] <- zzz_night$lengths[1] + 1 }
            ci_night[ii-5,jj] <- sum(zzz_night$lengths[zzz_night$values == TRUE]^2) / sum(zzz_night$lengths[zzz_night$values == TRUE])
            if(zzz_night$values[1] == FALSE) { 
                waso_night[ii-5,jj] <- sum(zzz_night$lengths[zzz_night$values == FALSE][-1]) 
            } else { waso_night[ii-5,jj] <- sum(zzz_night$lengths[zzz_night$values == FALSE]) }
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

## in specific time window
for(jj in 1:flyTable$experiment_interval[aa]) {
    ## subset per day
    temp_file <- dplyr::filter(sleep, day == jj)
    
    for(ii in 6:dim(sleep)[2]) {
        ## zt subset
        subset <- temp_file %>% dplyr::filter(dplyr::between(zt, flyTable$subset_start_zt[aa], flyTable$subset_end_zt[aa]))
        subset <- rle(dplyr::pull(subset[,ii]))
        subset <- dplyr::tibble(values = subset$values, lengths = subset$lengths) %>%
            dplyr::mutate(end = cumsum(lengths),
                          start = end - (lengths - 1),
                          zt_start = floor(start / 60))
        
        ## sleep bout amount
        ss_nBouts[ii-5,jj] <- sum(subset$values == TRUE)
        
        ## sleep bout lengths 
        temp_subset <- subset$lengths[subset$values == TRUE]
        ss_boutLength_df %<>% dplyr::bind_rows(dplyr::tibble(fly = rep(names(sleep)[ii], length(temp_subset)), 
                                                             phase = rep("Day", length(temp_subset)),
                                                             day = rep(jj, length(temp_subset)), 
                                                             lengths = temp_subset))
        
        ## sleep parameters
        if(sum(subset$values == TRUE) > 0) {
            ss_aveDurationBouts[ii-5,jj] <- round(mean(subset$lengths[subset$values == TRUE]), 2)
            ss_maxDurationBouts[ii-5,jj] <- max(subset$lengths[subset$values == TRUE])
            ss_maxDurationZT[ii-5,jj] <- subset %>% 
                dplyr::filter(values == TRUE) %>% dplyr::filter(lengths == max(lengths)) %>% 
                dplyr::slice(1) %>% dplyr::pull(zt_start) ## sleep latency
            if(zzz_day$values[1] == TRUE) { 
                ss_latency[ii-5,jj] <- 0 
            } else { ss_latency[ii-5,jj] <- subset$lengths[1] + 1 }
            ## consolidation index
            ss_ci[ii-5,jj] <- sum(subset$lengths[subset$values == TRUE]^2) / sum(subset$lengths[subset$values == TRUE])
            ## wake after sleep onset
            if(subset$values[1] == FALSE) { 
                ss_waso[ii-5,jj] <- sum(subset$lengths[subset$values == FALSE][-1]) 
            } else { ss_waso[ii-5,jj] <- sum(subset$lengths[subset$values == FALSE]) }
            ## brief awakenings default: 1-min awake epoch between two sleep sequences
            ss_briefAwake[ii-5,jj] = sum(subset$lengths[subset$values == FALSE] == 1) 
            ## brief awakenings following Koh K, Evans JM, Hendricks JC, Sehgal A (2006). PMID: 16938867
            ss_briefAwake_koh[ii-5,jj] = sum(subset$lengths[subset$values == FALSE] <= 4)
        } else {
            ss_aveDurationBouts[ii-5,jj] <- 0
            ss_maxDurationBouts[ii-5,jj] <- 0
            ss_maxDurationZT[ii-5,jj] <- NA
            ss_latency[ii-5,jj] <- 60 * (24 - flyTable$night_duration[aa])
            ss_ci[ii-5,jj] <- 0
            ss_waso[ii-5,jj] <- NA # or max score?
            ss_briefAwake[ii-5,jj] <- NA
            ss_briefAwake_koh[ii-5,jj] <- NA
        }
    }
}

if(tolower(flyTable$start_phase[1]) == "day") {
    nBouts <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(nBouts_day), dplyr::as_tibble(nBouts_night))
    aveDurationBouts <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(aveDurationBouts_day), dplyr::as_tibble(aveDurationBouts_night))
    maxDurationBouts <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(maxDurationBouts_day), dplyr::as_tibble(maxDurationBouts_night))
    maxDurationZT <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(maxDurationZT_day), dplyr::as_tibble(maxDurationZT_night))
    latency <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(latency_day), dplyr::as_tibble(latency_night))
    ci <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(ci_day), dplyr::as_tibble(ci_night))
    waso <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(waso_day), dplyr::as_tibble(waso_night))
    briefAwake <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(briefAwake_day), dplyr::as_tibble(briefAwake_night))
    briefAwake_koh <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(briefAwake_koh_day), dplyr::as_tibble(briefAwake_koh_night))
    boutLength_df %<>% dplyr::arrange(phase, day) 
} else {
    nBouts <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(nBouts_night), dplyr::as_tibble(nBouts_day))
    aveDurationBouts <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(aveDurationBouts_night), dplyr::as_tibble(aveDurationBouts_day))
    maxDurationBouts <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(maxDurationBouts_night), dplyr::as_tibble(maxDurationBouts_day))
    maxDurationZT <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(maxDurationZT_night), dplyr::as_tibble(maxDurationZT_day))
    latency <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(latency_night), dplyr::as_tibble(latency_day))
    ci <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(ci_night), dplyr::as_tibble(ci_day))
    waso <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(waso_night), dplyr::as_tibble(waso_day))
    briefAwake <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(briefAwake_night), dplyr::as_tibble(briefAwake_day))
    briefAwake_koh <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(briefAwake_koh_night), dplyr::as_tibble(briefAwake_koh_day))
    boutLength_df %<>% dplyr::arrange(dplyr::desc(phase), day)
}

ss_nBouts <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(ss_nBouts))
ss_aveDurationBouts <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(ss_aveDurationBouts))
ss_maxDurationBouts <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(ss_maxDurationBouts))
ss_maxDurationZT <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(ss_maxDurationZT))
ss_latency <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(ss_latency))
ss_ci <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(ss_ci))
ss_waso <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(ss_waso))
ss_briefAwake <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(ss_briefAwake))
ss_briefAwake_koh <- dplyr::bind_cols(fly_id = flies, dplyr::as_tibble(ss_briefAwake_koh))
ss_boutLength_df %<>% dplyr::arrange(dplyr::desc(phase), day) ### correct?

hist_day <- ggplot2::ggplot(dplyr::filter(boutLength_df, phase == "Day"), aes(lengths)) +
    ggplot2::geom_histogram(binwidth = 5) +
    ggplot2::geom_vline(xintercept = median(dplyr::filter(boutLength_df, phase == "Day")$lengths), color = "navyblue") +
    ggplot2::annotate("text", x = median(dplyr::filter(boutLength_df, phase == "Day")$lengths), y = 0, vjust = 0, 
                      label = median(dplyr::filter(boutLength_df, phase == "Day")$lengths)) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank()) +
    ggplot2::labs(title = paste(flyTable$import_file[aa], ":: Bout length distribution during day phase"))
hist_night <- ggplot2::ggplot(dplyr::filter(boutLength_df, phase == "Night"), aes(lengths)) +
    ggplot2::geom_histogram(binwidth = 5) +
    ggplot2::geom_vline(xintercept = median(dplyr::filter(boutLength_df, phase == "Night")$lengths), color = "navyblue") +
    ggplot2::annotate("text", x = median(dplyr::filter(boutLength_df, phase == "Night")$lengths), y = 0, vjust = 0, 
                      label = median(dplyr::filter(boutLength_df, phase == "Night")$lengths)) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank()) +
    ggplot2::labs(title = paste(flyTable$import_file[aa], ":: Bout length distribution during night phase"))
g_hist <- gridExtra::grid.arrange(hist_day, hist_night, nrow = 2)
ggplot2::ggsave(filename = paste0(filesDir, subDir, "sleep_bout_length.png"), g_hist, width = 8.58, height = 4.68)

# export --------------------------------------------------------

# write.table(sleep_day, paste0(filesDir, subDir, "sleep_pattern.txt"), quote = F, sep = "\t", row.names = F)
# write.table(sleep_phase, paste0(filesDir, subDir, "sleep_phase.txt"), quote = F, sep = "\t", row.names = F)
# write.table(nBouts, paste0(filesDir, subDir, "sleep_bout_n.txt"), sep = "\t", quote = F, row.names = F)
# write.table(aveDurationBouts, paste0(filesDir, subDir, "sleep_bout_aveDur.txt"), sep = "\t", quote = F, row.names = F)
# write.table(maxDurationBouts, paste0(filesDir, subDir, "sleep_bout_maxDur.txt"), sep = "\t", quote = F, row.names = F)
# write.table(maxDurationZT, paste0(filesDir, subDir, "sleep_bout_maxDur_ZT.txt"), sep = "\t", quote = F, row.names = F)
# write.table(latency, paste0(filesDir, subDir, "sleep_latency.txt"), sep = "\t", quote = F, row.names = F)
# write.table(ci, paste0(filesDir, subDir, "sleep_ciIndex.txt"), sep = "\t", quote = F, row.names = F)
# write.table(waso, paste0(filesDir, subDir, "sleep_waso.txt"), sep = "\t", quote = F, row.names = F)
# write.table(briefAwake, paste0(filesDir, subDir, "sleep_briefAwake.txt"), sep = "\t", quote = F, row.names = F)
# write.table(briefAwake_koh, paste0(filesDir, subDir, "sleep_briefAwake_koh.txt"), sep = "\t", quote = F, row.names = F)
# write.table(boutLength_df, paste0(filesDir, subDir, "sleep_bout_length.txt"), sep = "\t", quote = F, row.names = F)

write.table(ss_nBouts, paste0(filesDir, subDir, "sleep_bout_n_SUBSET.txt"), sep = "\t", quote = FALSE, row.names = FALSE)
write.table(ss_aveDurationBouts, paste0(filesDir, subDir, "sleep_bout_aveDur_SUBSET.txt"), sep = "\t", quote = FALSE, row.names = FALSE)
write.table(ss_maxDurationBouts, paste0(filesDir, subDir, "sleep_bout_maxDur_SUBSET.txt"), sep = "\t", quote = FALSE, row.names = FALSE)
write.table(ss_maxDurationZT, paste0(filesDir, subDir, "sleep_bout_maxDur_ZT_SUBSET.txt"), sep = "\t", quote = FALSE, row.names = FALSE)
write.table(ss_latency, paste0(filesDir, subDir, "sleep_latency_SUBSET.txt"), sep = "\t", quote = FALSE, row.names = FALSE)
write.table(ss_ci, paste0(filesDir, subDir, "sleep_ciIndex_SUBSET.txt"), sep = "\t", quote = FALSE, row.names = FALSE)
write.table(ss_waso, paste0(filesDir, subDir, "sleep_waso_SUBSET.txt"), sep = "\t", quote = FALSE, row.names = FALSE)
write.table(ss_briefAwake, paste0(filesDir, subDir, "sleep_briefAwake_SUBSET.txt"), sep = "\t", quote = FALSE, row.names = FALSE)
write.table(ss_briefAwake_koh, paste0(filesDir, subDir, "sleep_briefAwake_koh_SUBSET.txt"), sep = "\t", quote = FALSE, row.names = FALSE)
write.table(ss_boutLength_df, paste0(filesDir, subDir, "sleep_bout_lengthSUBSET.txt"), sep = "\t", quote = FALSE, row.names = FALSE)

writexl::write_xlsx(list(sleep_pattern = sleep_day,
                         sleep_pattern_30min = sleep_day_demi,
                         sleep_phase = sleep_phase,
                         sleep_bout_n = nBouts,
                         sleep_bout_aveDur = aveDurationBouts,
                         sleep_bout_maxDur = maxDurationBouts,
                         sleep_bout_maxDur_ZT = maxDurationZT,
                         sleep_bout_length = boutLength_df,
                         sleep_latency = latency,
                         sleep_ciIndex = ci, 
                         sleep_waso = waso,
                         sleep_briefAwake = briefAwake,
                         sleep_briefAwake_koh = briefAwake_koh),
                    paste0(filesDir, subDir, "_", flyTable$export_folder[aa], "_sleep.xlsx"))

rm(list = setdiff(ls(), c("sleep", "activity", "flyTable",
                          "filesDir", "subDir", "aa",
                          "zt_sequence", "day_sequence", "night_sequence",
                          "flies")))
