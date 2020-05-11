mobility <- function(dataset) {
  require(dplyr)
  require(countrycode)
  require(stringdist)
  mob <- read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=722f3143b586a83f") %>%
    rename(id.country = country_region, state=sub_region_1) %>%
    mutate(
      date=date %>% as.Date(),
      id.country = id.country %>% as.character() %>% countrycode("country.name", "ecb"),
      state = state %>% as.character()
    ) %>%
    filter(id.country %in% (dataset$country %>% unique() %>% countrycode("country.name", "ecb"))) %>%
    filter(is.na(sub_region_2) | grepl("^\\s*$", sub_region_2)) %>%
    mutate(state=ifelse(grepl("^\\s*$", state), NA, state)) %>%
    mutate(city=as.character(NA)) %>%
    select(-sub_region_2, -country_region_code)
  dataset %>%
    mutate(id.country = country %>% countrycode("country.name", "ecb")) %>%
    left_join(mob, by=c("id.country", "state", "city", "date")) %>%
    select(-id.country)
}