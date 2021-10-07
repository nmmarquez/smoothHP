rm(list=ls())
library(tidyverse)
library(data.table)
library(smoothHP)


# make projections
proj_results <- bind_rows(
    kc_pop_data[kc_pop_data$Year %in% c(2010,2005)] %>%
        arrange(GEOID, Sex, Race, Age5, Year) %>%
        group_by(GEOID, Sex, Race, Age5, County) %>%
        summarize(value = last(value)*2 - first(value), .groups = "drop") %>%
        mutate(Year = 2015, Method = "Linear"),
    multi_stage_group_HP_project(
        kc_pop_data[kc_pop_data$Year %in% c(2010,2005)],
        stages = list(c("County", "Race", "GEOID"))) %>%
        mutate(value = ifelse(!is.finite(value), 0, value)) %>%
        mutate(Method = "HP Method"),
    multi_stage_group_HP_project(
        kc_pop_data[kc_pop_data$Year %in% c(2010,2005)],
        stages = list("County", c("Race", "GEOID"))) %>%
        mutate(Method = "Single Stage\nSmoothing"),
    multi_stage_group_HP_project(
        kc_pop_data[kc_pop_data$Year %in% c(2010,2005)],
        stages = list("County", "Race", "GEOID")) %>%
        mutate(Method = "Multi Stage\nSmoothing"),
    multi_stage_group_HP_project(
        kc_pop_data[kc_pop_data$Year %in% c(2010,2005)],
        stages = list(c("County", "Race"), "GEOID")) %>%
        mutate(Method = "Race\nSmoothing"),
    multi_stage_group_HP_project(
        kc_pop_data[kc_pop_data$Year %in% c(2010,2005)],
        stages = list(c("County", "GEOID"), "Race")) %>%
        mutate(Method = "GEO\nSmoothing"),
    multi_stage_group_HP_project(
        kc_pop_data[kc_pop_data$Year %in% c(2010,2005)],
        stages = list("County", "GEOID", "Race")) %>%
        mutate(Method = "Multi Stage Geo\nSmoothing")) %>%
    filter(Year == 2015) %>%
    rename(projections = value)

calc_nll <- function(value, estimate){
    # this first one is the simple negative log likelihood
    # -sum(dpois(value, estimate, log = TRUE))
    # the second is a modification to account for non integer values
    # -sum(log((estimate^value * exp(-estimate))/factorial(value)))
    # the third works in log space to deal with large numbers
    -sum(value*log(estimate) - estimate - lfactorial(value))
}

proj_results %>%
    left_join(kc_pop_data) %>%
    mutate(projections = ifelse(projections <= 0, .01, projections)) %>%
    group_by(Race, Method) %>%
    summarise(res = calc_nll(value, projections)) %>%
    filter(res == min(res)) %>%
    print(n=100)


proj_results %>%
    left_join(kc_pop_data) %>%
    mutate(projections = ifelse(projections <= 0, .01, projections)) %>%
    group_by(Age5, Sex, Method) %>%
    summarise(res = calc_nll(value, projections)) %>%
    filter(res == min(res)) %>%
    print(n=1000)

proj_results %>%
    left_join(kc_pop_data) %>%
    filter(value!=0) %>%
    mutate(projections = ifelse(projections <= 0, .01, projections)) %>%
    group_by(Race, Method) %>%
    summarise(res = calc_MAPER(value, projections)) %>%
    filter(res == min(res)) %>%
    print(n=100)

proj_results %>%
    left_join(kc_pop_data) %>%
    filter(value!=0) %>%
    mutate(projections = ifelse(projections <= 0, .01, projections)) %>%
    group_by(Age5, Sex, Method) %>%
    summarise(res = calc_MAPE(value, projections)) %>%
    filter(res == min(res)) %>%
    print(n=1000)
