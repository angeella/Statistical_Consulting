down.covid <- function() {
  dataset <- 1:2 %>%
    lapply(COVID19::covid19, country=NULL) %>%
    bind_rows() %>%
    mutate(date = date %>% as.Date("%Y-%m-%d")) %>%
    as.data.frame() %>%
    mutate(
      id2 = administrative_area_level_2 %>%
        countrycode("country.name", "ecb", warn=F),
      id1 = administrative_area_level_1 %>%
        countrycode("country.name", "ecb", warn=F)
    ) %>%
    mutate(
      id1 = ifelse(is.na(id2), id1, id2)
    ) %>%
    select(-id2)
  # all(table(dataset$id, dataset$date, useNA = "ifany")<=1)
  dataset
}
