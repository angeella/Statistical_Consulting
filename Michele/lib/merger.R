
merger <- function(dataset, to.lag=c("confirmed")) { # dataset from COVID19::covid19(...)
  require(countrycode)
  require(dplyr)
  require(reshape2)
  require(tidyr)
  require(rvest) # for web harvesting
  require(RSQLite) # per maneggiare le tabelle come query, più semplice per alcune operazioni
  
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
  
  who <- who[,setdiff(colnames(who), c("Country.Region", "WHO.region"))]
  if (!isTRUE(all(table(who$id)==1))) warnings("attenzione, identificativo non univoco per i dati who")
  aux <- colnames(who) != "id"
  colnames(who)[aux] <- colnames(who)[aux] %>% as.Date("X%m.%d.%Y") %>% format()
  aux <- colnames(who)[aux]
  who <- who %>% reshape(ids = "id", timevar = "date", v.names = "who.cases", varying = list(aux), times=aux, direction = "long")
  rownames(who) <- NULL
  who$date <- as.Date(who$date, "%Y-%m-%d")
  
  dataset <- dataset %>% left_join(who, by=c("id", "date"))

  # 2. Hospital bed as variable from OECD (data folder);
  
  warnings(paste("citare: OECD (2020), Hospital beds (indicator). doi: 10.1787/0191328e-en (Accessed on 30 April 2020)"))
  
  oecd <- list.files(path="Data", pattern = "^DP_LIVE_.*.csv$", full.names = T) %>% lapply(read.csv)
  oecd <- oecd %>% do.call(what = "rbind") %>% distinct()
  oecd <- oecd[oecd %>% complete.cases(),]
  # oecd <- oecd[oecd$Flag.Codes=="",]
  warnings("quali sono le codifiche delle flag di OECD in DATA?")
  oecd <- oecd[oecd$SUBJECT %in% c("TOT", "ACUTE"), oecd %>% sapply(function(v) length(unique(v)) > 1)]
  oecd$SUBJECT <- factor(oecd$SUBJECT)
  oecd <- oecd[,c("LOCATION", "SUBJECT", "TIME", "Value", "Flag.Codes")]
  colnames(oecd) <- c("id", "type", "year", "beds", "flag")
  oecd$year <- as.numeric(oecd$year)
  
  mydb <- dbConnect(RSQLite::SQLite(), "")
  mydb %>% dbWriteTable("oecd", oecd, overwrite=T)
  
  oecd <- mydb %>% dbGetQuery('
                      SELECT id, type, beds
                      FROM oecd
                      WHERE (id, type, year) IN (
                        SELECT id, type, max(year)
                        FROM oecd
                        GROUP BY id, type
                      )')
  oecd <- oecd %>% dcast(id ~ type, value.var="beds")
  oecd <- oecd[,c("id", "ACUTE", "TOT")]
  colnames(oecd) <- c("id", "beds.icu", "beds.tot")
  
  dataset <- dataset %>% left_join(oecd, by="id")
  
  # 3. Reference lockdown across states: 
  # - https://en.wikipedia.org/wiki/National_responses_to_the_2019%E2%80%9320_coronavirus_pandemic
  
    # reply: scartato, però posso calcolare io i momenti di de/e-scalation delle misure
    # vedi alla fine della funzione
  
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
  
    # reply: reimplementato dalla reference successiva
  
  #   - https://github.com/OxCGRT/covid-policy-tracker/
  
  ox <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest_withnotes.csv")
  
  ox$Date <- ox$Date %>%
    sub(pattern = "^(\\d{4})(\\d{2})(\\d{2})$", replacement = "\\1-\\2-\\3") %>%
    as.Date("%Y-%m-%d")
  ox$CountryName <- NULL
  colnames(ox)[1:2] <- c("id", "date")
  
  colnames(ox) <- paste0("ox.", colnames(ox))
  
  dataset <- dataset %>% left_join(ox, by=c("id"="ox.id", "date"="ox.date"))
  
    # calcola le variazioni
  
  deltas <- dataset[,c("id", "date", to.lag)]
  deltas[,-c(1:2)] <- sapply(deltas[,-c(1:2)], function(v) v %>% as.character() %>% as.numeric())
  # pensavo di usare SQL qualora la frequenza degli aggiornamenti fosse variabile
  # sul mio il seguente codice schianta -- Michele.
  # deltas <- deltas %>% reshape(direction = "long",
  #                    idvar = c("id", "date"),
  #                    timevar= "policytype",
  #                    v.names = "policyvalue", varying = list(to.lag), times=to.lag)
  # deltas <- deltas[,c("id", "date", "policytype", "policyvalue")]
  # deltas <- deltas[deltas %>% complete.cases(),]
  deltas$date <- as.Date(deltas$date) # per assicurarmi che l'incremento di un giorno poi funzioni
  deltas.prev <- deltas
  deltas.prev$date <- deltas.prev$date + 1 # vedi prima
  colnames(deltas.prev) <- colnames(deltas.prev) %>% paste0(".diff")
  
  deltas <- deltas %>% left_join(deltas.prev, by=c("id"="id.diff", "date"="date.diff"))
  aux <- paste0(to.lag,".diff")
  deltas[is.na(deltas)] <- 0
  deltas[,aux] <- deltas[,to.lag] - deltas[,aux]
  deltas <- deltas[,c("id", "date", aux)]
  
  dataset <- dataset %>% left_join(deltas, by=c("id", "date"))
  
  return(dataset)
}