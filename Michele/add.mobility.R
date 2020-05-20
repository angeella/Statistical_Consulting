add.mobility <- function(dataset) {
  mobility <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=57b4ac4fc40528e2" %>%
    read.csv() %>%
    filter(is.na(sub_region_1) | grepl("^\\s*$", sub_region_1)) %>%
    mutate(
      date = date %>% as.Date("%Y-%m-%d"),
      id1 = country_region %>% countrycode("country.name", "ecb")
    )
  
  # all(table(mobility$id1, mobility$date, useNA="ifany") <= 1)
  
  dataset %>%
    left_join(mobility, by=c("id1", "date"))
}