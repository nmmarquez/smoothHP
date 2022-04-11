rm(list=ls())
library(tidyverse)
library(tidycensus)

var_names <- load_variables(2010, "sf1") %>%
    filter(str_starts(name, "P009")) %>%
    filter(
        name %in% c("P009002", str_c("P00900", 5:9), str_c("P0090", 10:11))) %>%
    select(variable = name, label) %>%
    mutate(label = sapply(label, function(z) last(unlist(str_split(z, "\\!")))))

var_names_2020 <- var_names %>%
    mutate(variable = str_c(str_replace(variable, "P009", "P2_"), "N"))

dec_df <- bind_rows(
    get_decennial("county", table = "P2_", year = 2020, state = "WA") %>%
        right_join(var_names_2020) %>%
        mutate(Year = 2020),

    get_decennial("county", table = "P009", year = 2010, state = "WA") %>%
        right_join(var_names) %>%
        mutate(Year = 2010)) %>%
    select(GEOID, NAME, label, Year, value)

dec_df %>%
    filter(GEOID == "53033") %>%
    ggplot(aes(x = Year, y = value, fill = label)) +
    geom_col()

dec_df %>%
    filter(GEOID == "53033") %>%
    arrange(label, Year) %>%
    group_by(label) %>%
    summarise(
        pinc = (last(value) - first(value))/first(value),
        inc = (last(value) - first(value)))

dec_df %>%
    filter(GEOID == "53033") %>%
    group_by(Year) %>%
    mutate(P = value/sum(value))
