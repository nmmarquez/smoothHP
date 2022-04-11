rm(list=ls())
library(tidyverse)
library(smoothHP)

full_df <- read_rds("~/Downloads/updated_tract_df.rds") %>%
    filter(str_starts(GEOID, "53033"))

full_projections <- function(df, start_year, end_year = start_year + 5){
    proj_results <- bind_rows(
        df %>%
            arrange(GEOID, Sex, Race, Age5, Year) %>%
            group_by(GEOID, Sex, Race, Age5, County) %>%
            summarize(
                value = last(value)*(last(value)/first(value)),
                .groups = "drop") %>%
            mutate(Year = end_year+5, Method = "Simplified"),
        df %>%
            arrange(GEOID, Sex, Race, Age5, Year) %>%
            group_by(GEOID, Sex, Race, Age5, County) %>%
            summarize(
                value = last(value)*((last(value)/first(value))*2),
                .groups = "drop") %>%
            mutate(Year = end_year+10, Method = "Simplified"),
        df %>%
            arrange(GEOID, Sex, Race, Age5, Year) %>%
            group_by(GEOID, Sex, Race, Age5, County) %>%
            summarize(
                value = last(value)*((last(value)/first(value))*3),
                .groups = "drop") %>%
            mutate(Year = end_year+15, Method = "Simplified"),
        multi_stage_group_HP_project(
            df,
            stages = list(c("County", "Race", "GEOID"))) %>%
            mutate(value = ifelse(!is.finite(value), 0, value)) %>%
            mutate(Method = "HP Method"),
        multi_stage_group_HP_project(
            df,
            stages = list("County", c("Race", "GEOID"))) %>%
            mutate(Method = "Single Stage\nSmoothing"),
        multi_stage_group_HP_project(
            df,
            stages = list("County", "Race", "GEOID")) %>%
            mutate(Method = "Multi Stage\nSmoothing")) %>%
        rename(projections = value) %>%
        mutate(start_year = start_year)
}

proj_df <- lapply(2000:2010, function(y){
    print(y)
    full_projections(full_df[full_df$Year %in% c(y, y+5)], y)
    }) %>%
    bind_rows()

age_results <- proj_df %>%
    mutate(year_out = Year - start_year-5) %>%
    left_join(full_df) %>%
    filter(value!=0) %>%
    filter(is.finite(projections)) %>%
    mutate(projections = ifelse(projections <= 0, .01, projections)) %>%
    group_by(Age5, year_out, Method) %>%
    summarise(res = calc_MAPER(value, projections)) %>%
    arrange(year_out, Age5, abs(res))

age_res_df <- age_results %>%
    mutate(Name = str_c(Method, " ", year_out)) %>%
    ungroup() %>%
    mutate(res2 = as.character(round(res, 2))) %>%
    group_by(Age5, year_out) %>%
    mutate(res2 = ifelse(
        res == min(res), str_c("\\color{green}{", res2, "}"), res2)) %>%
    mutate(res2 = ifelse(
        res == max(res), str_c("\\color{red}{", res2, "}"), res2)) %>%
    ungroup() %>%
    select(Age5, Name, res = res2) %>%
    mutate(Name = factor(
        Name, levels = c(
            str_c(
                rep(c(
                    "Simplified", "HP Method", "Single Stage\nSmoothing",
                    "Multi Stage\nSmoothing"), 3),
                rep(c(" 5", " 10", " 15"), each = 4))))) %>%
    arrange(Age5, Name) %>%
    pivot_wider(names_from = Name, values_from = res)

age_res_df %>%
    as.matrix() %>%
    apply(1, str_c, collapse = " & ") %>%
    sapply(function(z) str_c(z, " \\\\\n")) %>%
    sapply(cat)

print(age_results, n=1000)

race_results <- proj_df %>%
    mutate(year_out = Year - start_year-5) %>%
    left_join(full_df) %>%
    filter(value!=0) %>%
    filter(is.finite(projections)) %>%
    mutate(projections = ifelse(projections <= 0, .01, projections)) %>%
    group_by(Race, year_out, Method) %>%
    summarise(res = calc_MAPER(value, projections)) %>%
    arrange(year_out, Race, res)

print(race_results, n=1000)

race_res_df <- race_results %>%
    mutate(Name = str_c(Method, " ", year_out)) %>%
    ungroup() %>%
    mutate(res2 = as.character(round(res, 2))) %>%
    group_by(Race, year_out) %>%
    mutate(res2 = ifelse(
        res == min(res), str_c("\\color{green}{", res2, "}"), res2)) %>%
    mutate(res2 = ifelse(
        res == max(res), str_c("\\color{red}{", res2, "}"), res2)) %>%
    ungroup() %>%
    select(Race, Name, res = res2) %>%
    mutate(Name = factor(
        Name, levels = c(
            str_c(
                rep(c(
                    "Simplified", "HP Method", "Single Stage\nSmoothing",
                    "Multi Stage\nSmoothing"), 3),
                rep(c(" 5", " 10", " 15"), each = 4))))) %>%
    arrange(Race, Name) %>%
    pivot_wider(names_from = Name, values_from = res)

race_res_df %>%
    as.matrix() %>%
    apply(1, str_c, collapse = " & ") %>%
    sapply(function(z) str_c(z, " \\\\\n")) %>%
    sapply(cat)


all_results <- proj_df %>%
    mutate(year_out = Year - start_year-5) %>%
    left_join(full_df) %>%
    filter(value!=0) %>%
    mutate(projections = ifelse(projections <= 0, .01, projections)) %>%
    group_by(year_out, Method) %>%
    summarise(res = calc_MAPE(value, projections)) %>%
    arrange(year_out, res) %>%
    filter(1:n() == 1)

all_results
