rm(list=ls())
library(tidyverse)
library(data.table)
library(sf)
library(tigris)
library(smoothHP)

# get KC shape file cleaned
kc_geo_df <- tracts("WA", "King", class = "sf")
king_water <- area_water("WA", "King", class = "sf")
kc_clean_geo_df <- king_water %>%
    # combines all the shapes into a single shape
    st_union() %>%
    # cut out the difference between the two shapes
    {st_difference(kc_geo_df, .)}

# make projections
kc_proj_df <- multi_stage_group_HP_project(
    kc_pop_data, par_year = 2015, proj_year = 2015)

ann_growth_df <- kc_pop_data %>%
    bind_rows(kc_proj_df) %>%
    filter((Year == 2015 | Year == 2040) & as.numeric(Age5) >= 14) %>%
    group_by(GEOID, Year) %>%
    summarize(value = sum(value)) %>%
    arrange(GEOID, Year) %>%
    summarize(PG = ((last(value) - first(value))/first(value))/25*100)


ann_growth_df %>%
    mutate(`Percent\nChange` = cut(
        PG, c(-2, 0, 1, 3, 5, 20),
        labels = c(
            "0-2 Decline", "0-1 Growth", "1-3 Growth", "3-5 Growth",
            ">5 Growth"))) %>%
    {right_join(kc_clean_geo_df, ., by = "GEOID")} %>%
    ggplot(aes(fill = `Percent\nChange`)) +
    geom_sf() +
    scale_fill_brewer(palette = "Spectral") +
    theme_void() +
    ggtitle("Annualized Percent Growth of Age 65+ Population")

race_ann_growth_df <- kc_pop_data %>%
    bind_rows(kc_proj_df) %>%
    filter((Year == 2015 | Year == 2040)) %>%
    filter(Race %in% c("Asian", "Hispanic", "Two or More Races")) %>%
    group_by(GEOID, Year) %>%
    summarize(value = sum(value)) %>%
    arrange(GEOID, Year) %>%
    summarize(PG = ((last(value) - first(value))/first(value))/25*100)

race_ann_growth_df %>%
    mutate(`Percent\nChange` = cut(
        PG, c(-.5, 0, 1, 3, 5, 20),
        labels = c(
            "0-.5% Decline", "0-1% Growth", "1-3% Growth", "3-5% Growth",
            ">5% Growth"))) %>%
    {right_join(kc_clean_geo_df, ., by = "GEOID")} %>%
    ggplot(aes(fill = `Percent\nChange`)) +
    geom_sf() +
    scale_fill_brewer(palette = "Spectral") +
    theme_void() +
    labs(fill = "") +
    theme(legend.text = element_text(size = 20))
