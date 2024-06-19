library(tidyverse)
library(readxl)
namesFunction <- function(nms) {
  janitor::make_clean_names(nms, case = "upper_camel")}

consumption <- read_excel("AMECO2_consumption.XLSX",
                          .name_repair = namesFunction)

exports <- read_excel("AMECO9_import_export.XLSX",
                      .name_repair = namesFunction)

gdp <- read_excel("AMECO6_gdp.XLSX",
                  .name_repair = namesFunction)

#### The contribution of net exports (final private consumption) to growth is calculated by multiplying the annual growth rate of net exports (final private consumption) by the share of net exports (final private consumption) in GDP at t − 1. The values are period averages (1994–98; 1999–2003; 2004–7).

countries <- c("United Kingdom",
               "United States",
               "Ireland",
               "Germany",
               "Netherlands",
               "Denmark")

net_exports <- exports %>% 
  select(8:ncol(.)) %>% 
  filter(Title %in% c("Exports of goods and services at 2015 prices")) %>% 
  filter(Country %in% countries) %>% 
  pivot_longer(5:ncol(.),
               names_to = "year",
               values_to = "exports_2015_prices") %>% 
  select(-c(SubChapter,
            Title,
            Unit_2))%>% 
  mutate(exports_2015_prices = as.numeric(exports_2015_prices)) %>% 
  left_join(exports %>% 
              select(8:ncol(.)) %>% 
              filter(Title %in% c("Imports of goods and services at 2015 prices")) %>% 
              filter(Country %in% countries) %>% 
              pivot_longer(5:ncol(.),
                           names_to = "year",
                           values_to = "imports_2015_prices") %>% 
              select(-c(SubChapter,
                        Title,
                        Unit_2)) %>% 
              mutate(imports_2015_prices = as.numeric(imports_2015_prices)),
            by = join_by(Country, year)) %>% 
  mutate(net_exports = exports_2015_prices - imports_2015_prices) %>% 
  left_join(gdp %>% 
              select(8:ncol(.)) %>% 
              filter(Country %in% countries) %>% 
              filter(Title == "Contribution to the increase of GDP at constant prices of exports of goods and services :- including intra-EU trade") %>% 
              pivot_longer(5:ncol(.),
                           names_to = "year",
                           values_to = "export_contrib_gdp") %>% 
              select(-c(SubChapter,
                        Title,
                        Unit_2))%>% 
              mutate(export_contrib_gdp = as.numeric(export_contrib_gdp)),
            by = join_by(Country, year)) %>% 
  left_join(gdp %>% 
              select(8:ncol(.)) %>% 
              filter(Country %in% countries) %>% 
              filter(Title == "Contribution to the increase of GDP at constant prices of imports of goods and services :- including intra-EU trade") %>% 
              pivot_longer(5:ncol(.),
                           names_to = "year",
                           values_to = "import_contrib_gdp") %>% 
              select(-c(SubChapter,
                        Title,
                        Unit_2))%>% 
              mutate(import_contrib_gdp = as.numeric(import_contrib_gdp)),
            by = join_by(Country, year)) %>% 
  left_join(gdp %>% 
              select(8:ncol(.)) %>% 
              filter(Country %in% countries) %>%  
              filter(str_detect(tolower(Title), "private consumption")) %>% 
              pivot_longer(5:ncol(.),
                           names_to = "year",
                           values_to = "hh_cons_contrib_gdp") %>% 
              select(-c(SubChapter,
                        Title,
                        Unit_2)) %>% 
              mutate(hh_cons_contrib_gdp = as.numeric(hh_cons_contrib_gdp))) %>% 
  mutate(year = str_remove_all(year, "X"),
         year = as.numeric(year)) %>% 
  mutate(net_export_contrib = export_contrib_gdp + import_contrib_gdp) %>% 
  mutate(ne_growth_rate = (net_exports-lag(net_exports, 1))/lag(net_exports,1)) %>% 
  mutate(ne_contrib = ne_growth_rate*lag(net_export_contrib,1)) %>% 
  mutate(period = if_else(year %in% 1994:1998,
                          "Period 1",
                          if_else(year %in% 1999:2003,
                                  "Period 2",
                                  if_else(year %in% 2004:2007,
                                          "Period 3",
                                          ""))))

net_exports %>% 
  filter(!period == "") %>% 
  # filter(Country == "United Kingdom") %>%
  group_by(Country,
           period) %>% 
  summarise(average_ne_contrib_gdp = mean(ne_contrib),
            average_hh_cons_contrib_gdp = mean(hh_cons_contrib_gdp),
            .groups = "drop") %>% 
  arrange(period,
          Country)




