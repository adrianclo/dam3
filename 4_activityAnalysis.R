cat("-- analyzing activity parameters\n")

# overall analysis: beam crossings per 30 min / phase -----------------------------------------

activity %>%
    dplyr::select(-c(time, zt)) %>%
    dplyr::group_by(day, phase, zt_demi) %>%
    dplyr::summarise_all(sum) %>% dplyr::ungroup() -> activity_day

activity_day %>%
    dplyr::mutate(zt = floor(zt_demi)) %>% 
    # dplyr::mutate(graph_x = rep(seq(0,23.5,.5), times = flyTable$experiment_interval[aa])) %>% # extra line
    dplyr::select(day:zt_demi,
                  zt,
                  # graph_x, 
                  dplyr::contains("fly")) %>% # extra line
    tidyr::pivot_longer(names_to = "fly_id",
                        values_to = "activity", -c(day, phase, 
                                                   zt,
                                                   # graph_x,
                                                   zt_demi)) %>% 
    dplyr::group_by(day, zt, fly_id) %>% 
    dplyr::summarise(activity = sum(activity), .groups = "drop") %>% 
    ggplot2::ggplot(., aes(zt, activity)) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::geom_rect(mapping = aes(xmin = which(zt_sequence == head(night_sequence,1)) -1, 
                                     xmax = which(zt_sequence == tail(night_sequence,1)) -1+.50, ymin = 0, ymax = max(activity)), fill = "gray", alpha = 1/5) +
    ggplot2::geom_point(alpha = 1/10) +
    ggplot2::stat_summary(geom = "point", fun = mean, color = "blue") +
    ggplot2::stat_summary(geom = "errorbar", fun.data = mean_se, color = "blue", width = .1) +
    ggplot2::stat_summary(geom = "line", fun = mean, color = "blue") +
    ggplot2::labs(x = "ZT", y = "Beam crossings (n)", title = flyTable$import_file[aa]) +
    ggplot2::scale_x_continuous(breaks = seq(0,23,3),
                                labels = zt_sequence[seq(1,length(zt_sequence),3)]) + # extra line
    ggplot2::facet_grid(day ~ ., labeller = label_both)
ggplot2::ggsave(paste0(filesDir, subDir, "activity_pattern.png"), width = 8.58, height = 3.11)

activity_day %>%
    dplyr::select(-zt_demi) %>%
    dplyr::group_by(day, phase) %>%
    dplyr::summarise_all(sum) %>%
    dplyr::arrange(phase) %>% dplyr::mutate(phase = as.character(phase)) %>% 
    dplyr::bind_rows(activity_day %>%
                         dplyr::select(-c(phase, zt_demi)) %>%
                         dplyr::group_by(day) %>%
                         dplyr::summarise_all(sum) %>%
                         dplyr::mutate(phase = "TOTAL") %>%
                         dplyr::select(day, phase, dplyr::contains("fly"))) %>%
    dplyr::ungroup() -> activity_phase

# anticipation indices ------------------------------------------------------------------------

##########################################
##  WARNING: ONLY WITH 12:12 Day:Night  ##
##########################################

## WHEN ZT0-ZT11 == "Day" AND ZT12-ZT23 == "Night"
## Evening anticipation index = [ (ZT9 to ZT11) - (ZT6 to ZT8) ]  / (ZT6 to ZT11) -> take last 6 zts from day_sequence
## Morning anticipation index = [ (ZT21 to ZT23) - (ZT18 to Z20) ] / (ZT18 to ZT23) -> take last 6 zts from night_sequence

if(length(day_sequence) == 12) {
    activity %>%
        dplyr::select(-c(time, zt_demi, phase)) %>%
        dplyr::filter(dplyr::between(zt, day_sequence[7], day_sequence[12])) %>% # ~ filter(zt >= x & zt < y)
        dplyr::mutate(period = factor(dplyr::case_when(dplyr::between(zt, day_sequence[7], day_sequence[9]) ~ "pre",
                                                       dplyr::between(zt, day_sequence[10], day_sequence[12]) ~ "post"), 
                                      levels = c("pre", "post"))) %>%
        dplyr::select(-zt) %>% dplyr::group_by(day, period) %>% dplyr::summarise_all(sum) %>%
        dplyr::select(-period) %>% dplyr::ungroup() %>% dplyr::group_by(day) %>% dplyr::summarise_all(diff) %>%
        dplyr::mutate(period = "post_pre_diff") %>% dplyr::select(day, period, dplyr::contains("fly")) %>% dplyr::ungroup() %>% 
        dplyr::bind_rows(activity %>%
                             dplyr::select(-c(time, zt_demi, phase)) %>%
                             dplyr::filter(dplyr::between(zt, day_sequence[7], day_sequence[12])) %>%
                             dplyr::select(-zt) %>% dplyr::group_by(day) %>% dplyr::summarise_all(sum) %>%
                             dplyr::mutate(period = "sum_activity") %>% dplyr::select(day, period, dplyr::contains("fly"))) %>%
        dplyr::mutate(period = factor(period, levels = c("post_pre_diff", "sum_activity"))) %>%
        dplyr::arrange(day) -> evening
    as.data.frame(
        dplyr::select(dplyr::summarize_all(dplyr::group_by(evening, day), list(first = first)), dplyr::contains("fly")) / 
            dplyr::select(dplyr::summarize_all(dplyr::group_by(evening, day), list(last = last)), dplyr::contains("fly"))) %>%
        tidyr::gather(fly_id, index) %>% dplyr::mutate(day = rep(paste0("ev_anticip_", 1:flyTable$experiment_interval[1]), times = length(flies))) %>%
        tiyr::spread(day, index) %>% dplyr::mutate(fly_id = stringr::str_remove(fly_id, "_first")) -> evening_anticipation
    
    activity %>%
        dplyr::select(-c(time, zt_demi, phase)) %>%
        dplyr::filter(dplyr::between(zt, night_sequence[7], night_sequence[12])) %>%
        dplyr::mutate(period = factor(dplyr::case_when(dplyr::between(zt, night_sequence[7], night_sequence[9]) ~ "pre",
                                                       dplyr::between(zt, night_sequence[10], night_sequence[12]) ~ "post"), 
                                      levels = c("pre", "post"))) %>%
        dplyr::select(-zt) %>% dplyr::group_by(day, period) %>% dplyr::summarise_all(sum) %>%
        dplyr::select(-period) %>% dplyr::group_by(day) %>% dplyr::summarise_all(diff) %>%
        dplyr::mutate(period = "post_pre_diff") %>% dplyr::select(day, period, dplyr::contains("fly")) %>%
        dplyr::bind_rows(activity %>%
                             dplyr::select(-c(time, zt_demi, phase)) %>%
                             dplyr::filter(dplyr::between(zt, night_sequence[7], night_sequence[12])) %>%
                             dplyr::select(-zt) %>% dplyr::group_by(day) %>% dplyr::summarise_all(sum) %>%
                             dplyr::mutate(period = "sum_activity") %>% dplyr::select(day, period, dplyr::contains("fly"))) %>%
        dplyr::mutate(period = factor(period, levels = c("post_pre_diff", "sum_activity"))) %>%
        dplyr::arrange(day) -> morning
    as.data.frame(dplyr::select(dplyr::summarize_all(dplyr::group_by(morning, day), list(first = first)), dplyr::contains("fly")) / 
                      dplyr::select(dplyr::summarize_all(dplyr::group_by(morning, day), list(last = last)), dplyr::contains("fly"))) %>%
        tidyr::gather(fly_id, index) %>% dplyr::mutate(day = rep(paste0("mo_anticip_", 1:flyTable$experiment_interval[aa]), times = length(flies))) %>%
        tiyr::spread(day, index) %>% dplyr::mutate(fly_id = stringr::str_remove(fly_id, "_first")) -> morning_anticipation
    
    # anticipation_old <- merge(as.data.frame(select(summarize_all(group_by(morning, day), funs(first)), contains("fly")) /
    #                                select(summarize_all(group_by(morning, day), funs(last)), contains("fly"))) %>%
    #                  gather(fly_id, index) %>% mutate(day = rep(paste0("mo_anticip_", 1:flyTable$experiment_interval[aa]), times = length(flies))) %>%
    #                  spread(day, index),
    #              as.data.frame(
    #                  select(summarize_all(group_by(evening, day), funs(first)), contains("fly")) /
    #                      select(summarize_all(group_by(evening, day), funs(last)), contains("fly"))) %>%
    #                  gather(fly_id, index) %>% mutate(day = rep(paste0("ev_anticip_", 1:flyTable$experiment_interval[1]), times = length(flies))) %>%
    #                  spread(day, index), by = "fly_id")
    anticipation <- merge(morning_anticipation, evening_anticipation, by = "fly_id")
    write.table(anticipation, paste0(filesDir, subDir, "activity_anticipation.txt"), quote = FALSE, sep = "\t", row.names = FALSE)
}

# activity %>% 
#     filter(between(zt, night_sequence[7], night_sequence[12])) %>% 
#     select(day,zt,phase,fly_01:fly_04) %>% 
#     group_by(day,phase,zt) %>% 
#     summarise(fly_01 = sum(fly_01),fly_02 = sum(fly_02),fly_03 = sum(fly_03),fly_04 = sum(fly_04))


# active count (proxy for hyperactivity) ----------------------------------

active_counter <- function(data) {
    temp_1 <- data %>% 
        dplyr::select(dplyr::contains("fly")) %>% 
        dplyr::mutate_all(.funs = function(x) ifelse(x > 0, T, F))
    temp_2 <- apply(data %>% dplyr::select(dplyr::contains("fly")), 2, sum) / apply(temp_1, 2, sum)
    
    dplyr::tibble(
        fly_id = names(temp_2),
        active_count = temp_2
    )
}

active_count <- activity %>% 
    tidyr::nest(data = -c(day,phase)) %>% 
    dplyr::mutate(active_count = purrr::map(data, ~active_counter(.x))) %>% 
    tidyr::unnest(active_count) %>% 
    dplyr::select(-data)
active_count <- active_count %>% 
    dplyr::filter(phase == "Night") %>% 
    dplyr::mutate(phase = paste(phase, day, sep = "_")) %>% 
    dplyr::select(-day) %>% 
    tidyr::pivot_wider(names_from = phase, values_from = active_count) %>% 
    dplyr::bind_cols(
        active_count %>% 
            dplyr::filter(phase == "Day") %>% 
            dplyr::mutate(phase = paste(phase, day, sep = "_")) %>% 
            dplyr::select(-day) %>% 
            tidyr::pivot_wider(names_from = phase, values_from = active_count) %>% 
            dplyr::select(-fly_id)
    )

# export --------------------------------------------------------

# write.table(activity_phase, paste0(filesDir, subDir, "activity_phase.txt"), quote = F, sep = "\t", row.names = F)
# write.table(activity_day, paste0(filesDir, subDir, "activity_pattern.txt"), quote = F, sep = "\t", row.names = F)

writexl::write_xlsx(list(activity_phase = activity_phase,
                         activity_pattern = activity_day,
                         active_count = active_count),
                    paste0(filesDir, subDir, "_", flyTable$export_folder[aa], "_activity.xlsx"))

rm(list = setdiff(ls(), c("sleep", "activity", "flyTable",
                          "filesDir", "subDir", "aa",
                          "zt_sequence", "day_sequence", "night_sequence",
                          "flies")))
