rm(list=ls())
library(tidyverse)
library(smoothHP)
library(tigris)
library(plotly)
library(sf)
library(GGally)
library(jcolors)

king_water <- area_water("WA", "King", class = "sf")
king_sf <- tracts("WA", "King", class = "sf", year = 2010) %>%
    st_buffer(1e-5) %>%
    st_difference(st_union(king_water))

base_df <- read_rds("~/Downloads/updated_tract_df.rds") %>%
    filter(str_starts(GEOID, "53033")) %>%
    # remove empty tract
    filter(GEOID != "53033990100")

full_df <- read_rds("~/Downloads/updated_tract_df.rds") %>%
    filter(str_starts(GEOID, "53033")) %>%
    # remove empty tract
    filter(GEOID != "53033990100") %>%
    # remove tract with hi GQ populations
    filter(GEOID != "53033005301" & GEOID != "53033005302")

uw_df <- read_rds("~/Downloads/updated_tract_df.rds") %>%
    # remove tract with hi GQ populations
    filter(GEOID == "53033005301" | GEOID == "53033005302") %>%
    filter(Year == 2020) %>%
    rename(projections = value) %>%
    select(-Year) %>%
    mutate(start_year = 2015) %>%
    full_join(
        expand_grid(
            County = 1,
            Year = c(2025, 2030, 2035, 2040),
            Method = c(
                "Simplified", "HP Method",
                "Single Stage\nSmoothing", "Multi Stage\nSmoothing")
        )
    )


empty_geoids <- filter(full_df, Year %in% c(2020)) %>%
    group_by(GEOID, Year) %>%
    summarize(value = sum(value), .groups = "drop") %>%
    filter(value == 0) %>%
    pull(GEOID)

ccr_est_df <- bind_rows(
    multi_stage_CCR_estimates(
        full_df, stages = list(c("County", "Race", "GEOID"))) %>%
        mutate(Method = "HP Method"),
    multi_stage_CCR_estimates(
        full_df, stages = list("County", c("Race", "GEOID"))) %>%
        mutate(Method = "Single Stage\nSmoothing"),
    multi_stage_CCR_estimates(
        full_df, stages = list("County", "Race", "GEOID")) %>%
        mutate(Method = "Two Stage\nSmoothing")) %>%
    filter(Sex == "Male") %>%
    filter(Race %in% c("Asian", "Hispanic", "White")) %>%
    filter(Age5 == "25-29")

ccr_plot <- ccr_est_df %>%
    ggplot(aes(x = CCR, fill = Race, color = Race)) +
    geom_density(alpha = .4) +
    facet_wrap(~Method, scales = "free") +
    theme_classic(base_size = 20) +
    xlim(c(.5, 2)) +
    labs(y="") +
    scale_fill_manual(values = c(
        "Asian" = "#194D44", "Hispanic" = "#5B6DC8", "White" = "#6ACDC5")) +
    scale_color_manual(values = c(
        "Asian" = "#194D44", "Hispanic" = "#5B6DC8", "White" = "#6ACDC5")) +
    ggtitle(
        "CCR estimates by Race and Ethnicity",
        "Distributions of CCR for 20-24 year old males")

ggsave("~/Downloads/ccr_plot.png", ccr_plot, width = 12, height = 8)


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
        df %>%
            arrange(GEOID, Sex, Race, Age5, Year) %>%
            group_by(GEOID, Sex, Race, Age5, County) %>%
            summarize(
                value = last(value)*((last(value)/first(value))*4),
                .groups = "drop") %>%
            mutate(Year = end_year+20, Method = "Simplified"),
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
        mutate(start_year = start_year) %>%
        bind_rows(uw_df)
}

final_df <- full_projections(full_df[full_df$Year %in% c(2015, 2020)], 2015)

all_df <- final_df %>%
    mutate(value = ifelse(is.finite(projections), projections, 0)) %>%
    bind_rows(mutate(base_df, Method = "Data"))

county_df <- all_df %>%
    group_by(Year, Method) %>%
    summarize(value = sum(value)) %>%
    filter(Year <= 2040) %>%
    mutate(Method = factor(
        Method, levels = c(
            "Data", "Simplified", "HP Method",
            str_c(c("Single", "Multi"), " Stage\nSmoothing")
        )))

pop_all_plot <- county_df %>%
    ggplot(aes(x = Year, y = value/(1000000), color = Method)) +
    geom_line(size = 2) +
    ylim(c(0, 8)) +
    theme_classic(base_size = 20) +
    scale_color_manual(
        values = c("Data" = "#DE1A1A", "Simplified" = "#BBB53E",
          "HP Method" = "#2A297A",
          "Single Stage\nSmoothing" = "#995533",
          "Multi Stage\nSmoothing" = "#D590DA")
    ) +
    labs(y = "Population\n(in millions)") +
    ggtitle("King County Population Projections", "By Model")

ggsave("~/Downloads/model_compare.png", pop_all_plot, width = 12, height = 8)

race_df <- bind_rows(
    full_df %>%
        mutate(Method = "Single Stage\nSmoothing"),

    full_df %>%
        mutate(Method = "Multi Stage\nSmoothing"),

    all_df %>%
        filter(Method %in% str_c(c("Single", "Multi"), " Stage\nSmoothing")
        )) %>%
    group_by(Year, Method, Race) %>%
    summarize(value = sum(value)) %>%
    filter(Year <= 2040) %>%
    mutate(P = value / sum(value))

race_model_comp <- race_df %>%
    ggplot(aes(x = Year, y = P, fill = Race)) +
    geom_area() +
    geom_vline(xintercept = 2020, linetype = 2, size = .5) +
    facet_wrap(~Method) +
    theme_classic(base_size = 20) +
    scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, 1), expand = c(0,0)) +
    scale_x_continuous(limits = c(2000, 2040), expand = c(0,0)) +
    labs(y = "Percent") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_jcolors(palette = "pal8")

ggsave("~/Downloads/race_model_compare.png", race_model_comp, width = 12, height = 8)

aging_pop <- all_df %>%
    filter(Method %in% c("Multi Stage\nSmoothing", "Data")) %>%
    filter(Year <= 2040) %>%
    group_by(Race, Year, Age5) %>%
    summarize(value = sum(value), .groups = "drop") %>%
    mutate(Age = case_when(
        as.numeric(Age5) <= 3 ~ "0-14",
        as.numeric(Age5) >= 4 & as.numeric(Age5) <= 10 ~ "15-44",
        as.numeric(Age5) >= 11 & as.numeric(Age5) <= 13 ~ "45-64",
        as.numeric(Age5) >= 14 ~ "65+")) %>%
    group_by(Race, Year, Age) %>%
    summarize(value = sum(value), .groups = "drop_last") %>%
    mutate(P = value/sum(value)) %>%
    ungroup() %>%
    ggplot(aes(x = Year, y = P, color = Race)) +
    geom_line(size = 2) +
    geom_vline(xintercept = 2020, linetype = 2, size = .5) +
    theme_classic(base_size = 20) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_color_jcolors(palette = "pal8") +
    ylab("") +
    labs(color = "Race or\nEthnicity") +
    facet_wrap(~Age, scales = "free_y") +
    ggtitle("Population Distribution", "by Age, Race, and Ethnicity")

ggsave("~/Downloads/aging_compare.png", aging_pop, width = 12, height = 8)

map_all <- king_sf %>%
    rename(GEOID = GEOID10) %>%
    right_join(
        all_df %>%
            filter(Method %in% c("Multi Stage\nSmoothing", "Data")) %>%
            filter(Year %in% c(2020, 2040)) %>%
            filter(Race %in% c("Asian", "Black", "White", "Hispanic")) %>%
            group_by(Race, GEOID, Year) %>%
            summarize(value = sum(value)) %>%
            summarize(Pchange = (last(value) - first(value))/first(value)/20) %>%
            filter(!(GEOID %in% empty_geoids)) %>%
            mutate(Pchange = ifelse(is.na(Pchange), 0, Pchange)) %>%
            arrange(Race, Pchange) %>%
            ungroup() %>%
            mutate(Pchange = ifelse(Pchange > .1, .1, Pchange)) %>%
            mutate(Porder0 = (Pchange - mean(Pchange))/sd(Pchange)) %>%
            mutate(Porder = ifelse(Porder0 > 2, 2, Porder0)) %>%
            mutate(Porder = ifelse(Porder0 < -2, -2, Porder0)) %>%
            ungroup()

    ) %>%
    ggplot() +
    geom_sf(aes(fill = Pchange), size = .4) +
    facet_wrap(~Race) +
    scale_fill_distiller(palette = "Spectral", direction = 1) +
    theme_void(base_size = 20) +
    #theme(legend.position = "none") +
    ggtitle(
        "Population Growth by Race and Ethnicty",
        "All Age Groups"
    )

ggsave("~/Downloads/map_all_pop.png", map_all, width = 12, height = 8)

df_pchange <- all_df %>%
    filter(Age5 %in% c("0-4", "5-9", "10-15")) %>%
    filter(Method %in% c("Multi Stage\nSmoothing", "Data")) %>%
    filter(Year %in% c(2020, 2040)) %>%
    filter(Race %in% c("Asian", "Black", "White", "Hispanic")) %>%
    group_by(Race, GEOID, Year) %>%
    summarize(value = sum(value)) %>%
    summarize(Pchange = (last(value) - first(value))/first(value)/20) %>%
    filter(!(GEOID %in% empty_geoids)) %>%
    mutate(Pchange = ifelse(is.na(Pchange), 0, Pchange)) %>%
    arrange(Race, Pchange) %>%
    ungroup() %>%
    mutate(Pchange = ifelse(Pchange > .1, .1, Pchange)) %>%
    mutate(Porder0 = (Pchange - mean(Pchange))/sd(Pchange)) %>%
    mutate(Porder = ifelse(Porder0 > 2, 2, Porder0)) %>%
    mutate(Porder = ifelse(Porder0 < -2, -2, Porder0)) %>%
    ungroup()

df_pchange %>%
    select(Race, GEOID, Pchange) %>%
    pivot_wider(names_from = Race, values_from = Pchange) %>%
    select(-GEOID) %>%
    ggpairs()

map_young <- king_sf %>%
    rename(GEOID = GEOID10) %>%
    right_join(df_pchange) %>%
    ggplot() +
    geom_sf(aes(fill = Pchange), size = .4) +
    facet_wrap(~Race) +
    scale_fill_distiller(palette = "YlGn", direction = 1, labels = scales::label_percent()) +
    theme_void(base_size = 20) +
    labs(fill = "Annual\nGrowth\nPercent") +
    #theme(legend.position = "none") +
    ggtitle(
        "Population Growth by Race and Ethnicty",
        "Annual Growth Percent by Tract for Population Aged 0-14"
    )

ggsave("~/Downloads/map_young_pop.png", map_young, width = 12, height = 8)
