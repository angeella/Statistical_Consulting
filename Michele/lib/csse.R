
csse <- function(dataset) {
  require(rvest)
  require(stringr)
  require(dplyr)
  require(pbapply)
  require(countrycode)
  read_html("https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports") %>%
    html_nodes("td.content a") %>%
    html_attr("href") %>%
    str_subset(pattern = "\\d+-\\d+-\\d+\\.csv$") %>%
    sub(pattern = ".*CSSEGISandData/COVID-19", replacement = "https://github.com/CSSEGISandData/COVID-19") %>%
    sub(pattern = "/blob/", replacement = "/raw/") %>%
    pbsapply(function(url) {
      date <- url %>%
        sub(pattern = ".*(\\d+)-(\\d+)-(\\d+)\\.csv$", replacement = "\\3-\\1-\\2") %>%
        as.Date()
      tryCatch({
        url %>%
          read.csv() %>%
          filter(grepl("^\\s*$", Province_State)) %>%
          select(Combined_Key, Confirmed, Deaths, Recovered) %>%
          rename(id.country=Combined_Key, ox.confirmed=Confirmed, ox.deaths=Deaths, ox.recovered=Recovered) %>%
          mutate(date=date, state=as.character(NA), id.country=id.country %>% as.character() %>% countrycode("country.name", "ecb", warn = F))
      }, error=function(e) NULL)
    }, cl=parallel::detectCores()) %>%
    bind_rows() %>%
    right_join(
      dataset %>%
        mutate(id.country = country %>% as.character() %>% countrycode("country.name", "ecb")),
      by=c("id.country", "state", "date")
      ) %>%
    select(-id.country)
}
