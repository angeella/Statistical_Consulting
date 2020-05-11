oxford <- function(dataset) {
  require(dplyr)
  require(countrycode)
  read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv") %>%
    mutate(id.country=CountryName %>% as.character() %>% countrycode("country.name", "ecb")) %>%
    rename(date=Date) %>%
    select(-CountryCode, -CountryName) %>%
    mutate(state=as.character(NA), date=date %>% sub(pattern = "^(\\d{4})(\\d{2})(\\d{2})$", replacement = "\\1-\\2-\\3") %>% as.Date()) %>%
    right_join(
      dataset %>% mutate(id.country=country %>% as.character() %>% gsub(pattern=",.*", replacement="") %>% countrycode("country.name", "ecb")),
      by=c("id.country", "state", "date")
    ) %>%
    select(-id.country)
}
