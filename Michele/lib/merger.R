
merger <- function(dataset) { # dataset from COVID19::covid19(...)
  require(countrycode)
  require(dplyr)
  require(reshape2)
  require(tidyr)
  require(rvest) # for web harvesting
  # 1. Only confirmed cases from https://github.com/CSSEGISandData/COVID-19/tree/master/who_covid_19_situation_reports;
  
  who <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/who_covid_19_situation_reports/who_covid_19_sit_rep_time_series/who_covid_19_sit_rep_time_series.csv")
  
  who <- who[who$Province.States=="", setdiff(colnames(who), "Province.States")]
  who$id <- who$Country.Region %>% countrycode(origin = "country.name", destination = "iso3c")
  
  warnings("Adottato identificativo del Kosovo dal dataset del covid")
  who$id[who$Country.Region=="Kosovo"] <- dataset$id[grepl("Kosovo", dataset$country)][1]
  
  warnings("Dato duplicato Saint Martin, Sint Marteen, eliminato il secondo")
  who$id[who$Country.Region=="Saint Martin"] <- who$id[grepl("S*Maart", who$Country.Region)]
  who <- who[!grepl("S*Maart", who$Country.Region),]
  
  print(isTRUE(all(!is.na(who$id))))
  
  who <- who[,-c(1:2)]
  if (!isTRUE(all(table(who$id)==1))) warnings("attenzione, identificativo non univoco per i dati who")
  colnames(who) <- sub("^.*?(\\d+)\\.(\\d+)\\.(\\d+).*?$","\\3-\\1-\\2", colnames(who))
  aux <- colnames(who)!="id"
  colnames(who)[aux] <- format(as.Date(colnames(who)[aux], "%Y-%m-%d"))
  who <- reshape(who, ids = "id", timevar = "date", v.names = "whocases", varying = list(colnames(who)[aux]), times=colnames(who)[aux], direction = "long")
  head(who)
  rownames(who) <- NULL
  who$date <- as.Date(who$date, "%Y-%m-%d")
  
  dataset <- dataset %>% left_join(who, by=c("id", "date"))

  # 2. Hospital bed as variable from OECD (data folder);
  
  warnings(paste("citare: OECD (2020), Hospital beds (indicator). doi: 10.1787/0191328e-en (Accessed on 30 April 2020)"))
  
  oecd <- list.files(path="Data", pattern = "^DP_LIVE_.*.csv$", full.names = T) %>% lapply(read.csv)
  oecd <- do.call("rbind", oecd) %>% distinct()
  # oecd <- oecd[oecd$Flag.Codes=="",]
  warnings("quali sono le codifiche delle flag di OECD in DATA?")
  oecd <- oecd[,sapply(oecd, function(v) length(unique(v)) > 1)]
  oecd <- oecd[oecd$SUBJECT %in% c("TOT", "ACUTE"),]
  oecd$SUBJECT <- factor(oecd$SUBJECT)
  oecd <- oecd %>% group_by(LOCATION, SUBJECT) %>% summarise(TIME=max(TIME)) %>% left_join(oecd, by=c("LOCATION", "SUBJECT", "TIME"))
  colnames(oecd)
  oecd <- oecd %>% dcast(LOCATION ~ SUBJECT, value.var="Value")
  oecd <- oecd[,c("LOCATION", "TOT", "ACUTE")]
  colnames(oecd) <- c("id", "beds.total", "beds.icu")
  dataset <- dataset %>% left_join(oecd, by="id")
  
  # 3. Reference lockdown across states: 
  # - https://en.wikipedia.org/wiki/National_responses_to_the_2019%E2%80%9320_coronavirus_pandemic
  
  wiki <- read_html("https://en.wikipedia.org/wiki/National_responses_to_the_2019%E2%80%9320_coronavirus_pandemic")
  wiki <- wiki %>% html_nodes("table")
  wiki <- wiki[grepl("ntries.*erritori.*date.*evel", wiki %>% html_text())] %>% html_table(fill = T)
  wiki <- wiki[[1]]
  colnames(wiki) <- wiki[1,]
  wiki <- wiki[-1,]
  head(wiki)
  colnames(wiki) <- c("country", "subid", "lock.start", "lock.end", "lock.level")
  wiki$id <- wiki$country %>% countrycode(origin = "country.name", destination = "iso3c")
  warnings("Il Kosovo e' stato gestito (ancora)")
  wiki$id[grepl("Kosovo", wiki$country)] <- dataset$id[grepl("Kosovo", dataset$country)][1]
  isTRUE(all(!is.na(wiki$id)))
  
  wiki$subid[wiki$subid==wiki$country] <- NA
  warnings("scarto tutte le informazioni relative a regioni di nazioni")
  wiki <- wiki[is.na(wiki$subid), colnames(wiki)!="subid"]
  wiki <- wiki[-c(nrow(wiki)),]
  wiki$lock.start <- as.Date(sub("^.*?(\\d+-\\d+-\\d+).*?$","\\1", wiki$lock.start), "%Y-%m-%d")
  wiki$lock.end   <- as.Date(sub("^.*?(\\d+-\\d+-\\d+).*?$","\\1", wiki$lock.end  ), "%Y-%m-%d")
  wiki$lock.level <- gsub("\\s*\\[[a-z]+\\]\\s*", "", wiki$lock.level)
  
  wiki <- wiki[,colnames(wiki)!="country"]
  
  dataset <- dataset %>% left_join(wiki, by="id")
  
  # - https://www.politico.eu/article/europes-coronavirus-lockdown-measures-compared/
  
    ## reply: fonte verbosa, non inclusa
  
  # - https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker
  
    ## reply: usa i nostri stessi dati
  
  # - https://www.businessinsider.com/countries-on-lockdown-coronavirus-italy-2020-3?IR=T
  
    ## reply: articoli di giornale
  
  # - https://en.wikipedia.org/wiki/Stay-at-home_order#2020_coronavirus_pandemic
  
    ## reply: fonte verbosa
  
  # - https://en.wikipedia.org/wiki/Curfews_and_lockdowns_related_to_the_2019%E2%80%9320_coronavirus_pandemic
  
    ## reply: usa gli stessi dati dell'altra pagina di wikipedia gia' gestita prima
  
  # - https://www.cdc.gov/coronavirus/2019-ncov/php/risk-assessment.html
  
    ## reply: cosa devo fare con la fonte www.cdc.gov/coronavirus ?
  
  # 4. State Ranking of cases: 
  #   - https://www.worldometers.info/coronavirus/
  
  wrldmtr <- read_html("https://www.worldometers.info/coronavirus/") %>%
    html_node("table#main_table_countries_today") %>%
    html_table()
  
  wrldmtr <- wrldmtr[,c("Country,Other", "TotalCases", "TotalDeaths", "TotalRecovered", "ActiveCases", "Serious,Critical", "Deaths/1M pop", "TotalTests", "Tests/1M pop")]
  colnames(wrldmtr) <- c("country", paste0("wm.", c("cases.total", "deaths.total", "recovered.total", "cases.active", "serious.critical", "deaths/1Mpop", "tests.tot", "tests/1Mpop")))
  wrldmtr <- wrldmtr[,c("country", sort(colnames(wrldmtr)[-1]))]
  wrldmtr$id <- wrldmtr$country %>% countrycode(origin = "country.name", destination = "iso3c")
  wrldmtr <- wrldmtr[wrldmtr$country!="Saint Martin",] # duplicato come prima
  wrldmtr$country <- NULL
  
  aux <- setdiff(colnames(wrldmtr), "id")
  wrldmtr[,aux] <- wrldmtr[,aux] %>% sapply(as.character) %>% gsub(pattern = ",", replacement = "") %>% as.numeric()
  
  dataset <- dataset %>% left_join(wrldmtr, by="id")
  
  #   Data:
  #   - https://covidtracker.bsg.ox.ac.uk/
  #   - https://github.com/OxCGRT/covid-policy-tracker/
  
  return(dataset)
}