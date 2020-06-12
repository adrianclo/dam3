cat("-- analyzing activity parameters\n")

# overall analysis: beam crossings per 30 min / phase -----------------------------------------

# activity %>%
#     filter(day == 1 & phase == "Day" & between(zt,6,11)) %>%
#     mutate(phase = factor(case_when(between(zt,6,8) ~ "baseline", # first 3 hours
#                              between(zt,9,11) ~ "anticipation"), # last 3 hours
#                           levels = c("baseline","anticipation"))) %>%
#     select(time, phase, contains("fly")) %>%
#     gather(fly_id, activity, -c(time,phase)) %>%
#     ggplot(aes(time, activity, group = fly_id, color = fly_id)) +
#     facet_grid(phase ~ .) +
#     geom_line() +
#     scale_x_datetime(date_breaks = "2 hour")

activity %>%
    select(-c(time, zt)) %>%
    group_by(day, phase, zt_demi) %>%
    summarise_all(sum) %>% ungroup() -> activity_day

activity_day %>%
    mutate(graph_x = rep(seq(0,23.5,.5), times = flyTable$experiment_interval[aa])) %>% # extra line
    select(day:zt_demi, graph_x, contains("fly")) %>% # extra line
    gather(fly_id, activity, -c(day, phase, zt_demi, graph_x)) %>%
    ggplot(aes(graph_x, activity)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_rect(mapping = aes(xmin = which(zt_sequence == head(night_sequence,1)) -1, 
                            xmax = which(zt_sequence == tail(night_sequence,1)) -1+.50, ymin = 0, ymax = max(activity)), fill = "gray", alpha = 1/5) +
    geom_point(alpha = 1/10) +
    # geom_point(data = filter(activity_day, day == 1), aes(zt_demi,fly_01), color = "red") + # subset()
    stat_summary(geom = "point", fun.y = mean, color = "blue") +
    stat_summary(geom = "errorbar", fun.data = mean_se, color = "blue", width = .1) +
    stat_summary(geom = "line", fun.y = mean, color = "blue") +
    labs(x = "ZT", y = "Beam crossings (n)", title = flyTable$import_file[aa]) +
    scale_x_continuous(breaks = seq(0,23,3),
                       labels = zt_sequence[seq(1,length(zt_sequence),3)]) + # extra line
    facet_grid(day ~ ., labeller = label_both)
ggsave(paste0(filesDir, subDir, "activity_pattern.png"), width = 8.58, height = 3.11)

activity_day %>%
    select(-zt_demi) %>%
    group_by(day, phase) %>%
    summarise_all(sum) %>%
    arrange(phase) %>% mutate(phase = as.character(phase)) %>% 
    bind_rows(activity_day %>%
                  select(-c(phase, zt_demi)) %>%
                  group_by(day) %>%
                  summarise_all(sum) %>%
                  mutate(phase = "TOTAL") %>%
                  select(day, phase, contains("fly"))) %>%
    ungroup() -> activity_phase

# anticipation indices ------------------------------------------------------------------------

##########################################
##  WARNING: ONLY WITH 12:12 Day:Night  ##
##########################################

## WHEN ZT0-ZT11 == "Day" AND ZT12-ZT23 == "Night"
## Evening anticipation index = [ (ZT9 to ZT11) - (ZT6 to ZT8) ]  / (ZT6 to ZT11) -> take last 6 zts from day_sequence
## Morning anticipation index = [ (ZT21 to ZT23) - (ZT18 to Z20) ] / (ZT18 to ZT23) -> take last 6 zts from night_sequence

if(length(day_sequence) == 12) {
    activity %>%
        select(-c(time, zt_demi, phase)) %>%
        filter(between(zt, day_sequence[7], day_sequence[12])) %>% # ~ filter(zt >= x & zt < y)
        mutate(period = factor(case_when(between(zt, day_sequence[7], day_sequence[9]) ~ "pre",
                                         between(zt, day_sequence[10], day_sequence[12]) ~ "post"), 
                               levels = c("pre", "post"))) %>%
        select(-zt) %>% group_by(day, period) %>% summarise_all(sum) %>%
        select(-period) %>% ungroup() %>% group_by(day) %>% summarise_all(diff) %>%
        mutate(period = "post_pre_diff") %>% select(day, period, contains("fly")) %>% ungroup() %>% 
        bind_rows(activity %>%
                      select(-c(time, zt_demi, phase)) %>%
                      filter(between(zt, day_sequence[7], day_sequence[12])) %>%
                      select(-zt) %>% group_by(day) %>% summarise_all(sum) %>%
                      mutate(period = "sum_activity") %>% select(day, period, contains("fly"))) %>%
        mutate(period = factor(period, levels = c("post_pre_diff", "sum_activity"))) %>%
        arrange(day) -> evening
    as.data.frame(
        select(summarize_all(group_by(evening, day), list(first = first)), contains("fly")) / 
            select(summarize_all(group_by(evening, day), list(last = last)), contains("fly"))) %>%
        gather(fly_id, index) %>% mutate(day = rep(paste0("ev_anticip_", 1:flyTable$experiment_interval[1]), times = length(flies))) %>%
        spread(day, index) %>% mutate(fly_id = str_remove(fly_id, "_first")) -> evening_anticipation

    activity %>%
        select(-c(time, zt_demi, phase)) %>%
        filter(between(zt, night_sequence[7], night_sequence[12])) %>%
        mutate(period = factor(case_when(between(zt, night_sequence[7], night_sequence[9]) ~ "pre",
                                         between(zt, night_sequence[10], night_sequence[12]) ~ "post"), 
                               levels = c("pre", "post"))) %>%
        select(-zt) %>% group_by(day, period) %>% summarise_all(sum) %>%
        select(-period) %>% group_by(day) %>% summarise_all(diff) %>%
        mutate(period = "post_pre_diff") %>% select(day, period, contains("fly")) %>%
        bind_rows(activity %>%
                      select(-c(time, zt_demi, phase)) %>%
                      filter(between(zt, night_sequence[7], night_sequence[12])) %>%
                      select(-zt) %>% group_by(day) %>% summarise_all(sum) %>%
                      mutate(period = "sum_activity") %>% select(day, period, contains("fly"))) %>%
        mutate(period = factor(period, levels = c("post_pre_diff", "sum_activity"))) %>%
        arrange(day) -> morning
    as.data.frame(select(summarize_all(group_by(morning, day), list(first = first)), contains("fly")) / 
                      select(summarize_all(group_by(morning, day), list(last = last)), contains("fly"))) %>%
        gather(fly_id, index) %>% mutate(day = rep(paste0("mo_anticip_", 1:flyTable$experiment_interval[aa]), times = length(flies))) %>%
        spread(day, index) %>% mutate(fly_id = str_remove(fly_id, "_first")) -> morning_anticipation
    
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
    write.table(anticipation, paste0(filesDir, subDir, "activity_anticipation.txt"), quote = F, sep = "\t", row.names = F)
}

# activity %>% 
#     filter(between(zt, night_sequence[7], night_sequence[12])) %>% 
#     select(day,zt,phase,fly_01:fly_04) %>% 
#     group_by(day,phase,zt) %>% 
#     summarise(fly_01 = sum(fly_01),fly_02 = sum(fly_02),fly_03 = sum(fly_03),fly_04 = sum(fly_04))

# export --------------------------------------------------------

# write.table(activity_phase, paste0(filesDir, subDir, "activity_phase.txt"), quote = F, sep = "\t", row.names = F)
# write.table(activity_day, paste0(filesDir, subDir, "activity_pattern.txt"), quote = F, sep = "\t", row.names = F)

writexl::write_xlsx(list(activity_phase = activity_phase,
                         activity_pattern = activity_day),
                    paste0(filesDir, subDir, "_", flyTable$export_folder[aa], "_activity.xlsx"))

rm(list = setdiff(ls(), c("sleep", "activity", "flyTable",
                          "filesDir", "subDir", "aa",
                          "zt_sequence", "day_sequence", "night_sequence",
                          "flies")))

