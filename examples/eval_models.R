rm(list=ls())
library(tidyverse)
library(data.table)
library(smoothHP)
library(sf)
library(tigris)


sf_plot <- tracts("WA", "King", class = "sf")

sub_sf <- sf_plot %>%
    filter(INTPTLON > -122) %>%
    filter(as.numeric(INTPTLAT) > 47.4)

race_p <- kc_pop_data %>%
    filter(Year == 2015) %>%
    group_by(GEOID, Race) %>%
    summarize(value = sum(value)) %>%
    mutate(P = value/sum(value)) %>%
    filter(Race != "Two or More Races")

pop_growth <- kc_pop_data %>%
    filter(Year %in% c(2010, 2015)) %>%
    group_by(GEOID, Year) %>%
    summarise(value = sum(value)) %>%
    arrange(GEOID, Year) %>%
    summarise(
        Pinc = (last(value)-first(value))/first(value),
        Ninc = last(value)-first(value))

sub_sf %>%
    left_join(race_p) %>%
    filter(!is.na(P)) %>%
    ggplot() +
    geom_sf(aes(fill=P)) +
    facet_wrap(~Race) +
    scale_fill_distiller(palette = "Spectral")

race_p %>%
    left_join(pop_growth) %>%
    ggplot(aes(x=P, y=Pinc)) +
    geom_point() +
    facet_wrap(~Race)

sub_sf %>%
    left_join(pop_growth) %>%
    filter(!is.na(Pinc)) %>%
    ggplot() +
    geom_sf(aes(fill=log(Ninc))) +
    scale_fill_distiller(palette = "Spectral")

asian_p %>%
    left_join(pop_growth) %>%
    ggplot(aes(x=P, y=Pinc)) +
    geom_point()


start_year <- 2000
end_year <- 2005

kc_pop_data


# make projections
proj_results <- bind_rows(
    kc_pop_data[kc_pop_data$Year %in% c(end_year,start_year)] %>%
        arrange(GEOID, Sex, Race, Age5, Year) %>%
        group_by(GEOID, Sex, Race, Age5, County) %>%
        summarize(value = last(value)*2 - first(value), .groups = "drop") %>%
        mutate(Year = end_year+5, Method = "Linear"),
    kc_pop_data[kc_pop_data$Year %in% c(end_year,start_year)] %>%
        arrange(GEOID, Sex, Race, Age5, Year) %>%
        group_by(GEOID, Sex, Race, Age5, County) %>%
        summarize(value = last(value)*3 - first(value)*2, .groups = "drop") %>%
        mutate(Year = end_year+10, Method = "Linear"),
    kc_pop_data[kc_pop_data$Year %in% c(end_year,start_year)] %>%
        arrange(GEOID, Sex, Race, Age5, Year) %>%
        group_by(GEOID, Sex, Race, Age5, County) %>%
        summarize(value = last(value)*4 - first(value)*3, .groups = "drop") %>%
        mutate(Year = end_year+15, Method = "Linear"),
    multi_stage_group_HP_project(
        kc_pop_data[kc_pop_data$Year %in% c(end_year,start_year)],
        stages = list(c("County", "Race", "GEOID"))) %>%
        mutate(value = ifelse(!is.finite(value), 0, value)) %>%
        mutate(Method = "HP Method"),
    multi_stage_group_HP_project(
        kc_pop_data[kc_pop_data$Year %in% c(end_year,start_year)],
        stages = list("County", c("Race", "GEOID"))) %>%
        mutate(Method = "Single Stage\nSmoothing"),
    multi_stage_group_HP_project(
        kc_pop_data[kc_pop_data$Year %in% c(end_year,start_year)],
        stages = list("County", "Race", "GEOID")) %>%
        mutate(Method = "Multi Stage\nSmoothing"),
    multi_stage_group_HP_project(
        kc_pop_data[kc_pop_data$Year %in% c(end_year,start_year)],
        stages = list(c("County", "Race"), "GEOID")) %>%
        mutate(Method = "Race\nSmoothing"),
    multi_stage_group_HP_project(
        kc_pop_data[kc_pop_data$Year %in% c(end_year,start_year)],
        stages = list(c("County", "GEOID"), "Race")) %>%
        mutate(Method = "GEO\nSmoothing"),
    multi_stage_group_HP_project(
        kc_pop_data[kc_pop_data$Year %in% c(end_year,start_year)],
        stages = list("County", "GEOID", "Race")) %>%
        mutate(Method = "Multi Stage Geo\nSmoothing")) %>%
    rename(projections = value)

kc_pop_data %>%
    group_by(Year, Race) %>%
    summarize(value = sum(value)) %>%
    mutate(Method = "Data") %>%
    bind_rows(
        proj_results %>%
            group_by(Method, Year, Race) %>%
            summarize(value = sum(projections)) %>%
            filter(Year <= 2020)
    ) %>%
    filter(Race %in% c("Hispanic", "Asian", "Black", "White")) %>%
    filter(Method %in% c(
        "Data", "HP Method",
        "Single Stage\nSmoothing", "Multi Stage Geo\nSmoothing")) %>%
    filter(Year <= 2015) %>%
    ggplot(aes(x=Year, y=value, color = Method)) +
    geom_point(size=2) +
    geom_line() +
    facet_wrap(~Race, scales = "free_y") +
    theme_classic(base_size = 20) +
    scale_y_continuous(label=scales::comma) +
    labs(y = "Population", x="")

kc_pop_data %>%
    group_by(Year, Race) %>%
    summarize(value = sum(value)) %>%
    mutate(Method = "Data") %>%
    bind_rows(
        proj_results %>%
            group_by(Method, Year, Race) %>%
            summarize(value = sum(projections)) %>%
            filter(Year <= 2020)
    ) %>%
    filter(Race != "Two or More Races") %>%
    filter(Method != "Two or More Races") %>%
    ggplot(aes(x=Year, y=value, color = Method)) +
    geom_point() +
    facet_wrap(~Race, scales = "free_y")

kc_pop_data %>%
    group_by(Year, Race) %>%
    summarize(value = sum(value)) %>%
    mutate(Method = "Data") %>%
    bind_rows(
        proj_results %>%
            group_by(Method, Year, Race) %>%
            summarize(value = sum(projections)) %>%
            filter(Year <= 2020)
        ) %>%
    filter(Race != "Two or More Races") %>%
    filter(Method != "Two or More Races") %>%
    ggplot(aes(x=Year, y=value, color = Method)) +
    geom_point() +
    facet_wrap(~Race, scales = "free_y")

calc_nll <- function(value, estimate){
    # this first one is the simple negative log likelihood
    # -sum(dpois(value, estimate, log = TRUE))
    # the second is a modification to account for non integer values
    # -sum(log((estimate^value * exp(-estimate))/factorial(value)))
    # the third works in log space to deal with large numbers
    -sum(value*log(estimate) - estimate - lfactorial(value))
}


proj_results %>%
    filter(Year == 2015) %>%
    left_join(
        kc_pop_data %>%
            mutate(Year = ifelse(Year == 2019, 2020, Year))) %>%
    filter(value!=0) %>%
    #filter(Method != "Linear") %>%
    mutate(projections = ifelse(projections <= 0, .01, projections)) %>%
    group_by(Method, GEOID) %>%
    summarise(res = calc_nll(value, projections)) %>%
    summarise(res = median(res))
    filter(res == mea(res)) %>%
    print(n=100)

proj_results %>%
    left_join(kc_pop_data) %>%
    mutate(projections = ifelse(projections <= 0, .01, projections)) %>%
    group_by(GEOID, Method) %>%
    summarise(
        value = sum(value, na.rm = T),
        projections = sum(projections, na.rm = T)) %>%
    group_by(GEOID, Method) %>%
    mutate(res = calc_nll(value, projections)) %>%
    group_by(GEOID) %>%
    filter(res == min(res)) %>%
    group_by(Method) %>%
    summarise(N = n())

proj_results %>%
    filter(Method != "Linear") %>%
    left_join(kc_pop_data) %>%
    mutate(projections = ifelse(projections <= 0, .01, projections)) %>%
    group_by(Age5, Sex, Method) %>%
    summarise(res = calc_nll(value, projections)) %>%
    filter(res == min(res)) %>%
    print(n=1000)


