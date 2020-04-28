
require(rvest)

# https://github.com/CSSEGISandData/COVID-19

## https://github.com/CSSEGISandData/COVID-19/tree/master/archived_data
# deprecated

## https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data

### https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports
### csv giornalieri, con casi|morti|guariti in colonna delle varie nazioni in riga
### esclude gli USA

### https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports_us
### csv giornalieri, solo per gli stati federali degli USA, da inizio aprile a oggi
### include ospedalizzati

### https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
### csv separati per:
### casi negli USA
### casi nel resto del mondo
### morti negli USA
### morti nel resto del mondo
### 

urls <- read_html("https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series") %>% html_nodes("td.content a") %>% html_attr("href")
urls <- urls[grepl("csv$", urls)]
urls <- sub("^.*?CSSEGISandData/COVID-19", "https://github.com/CSSEGISandData/COVID-19", urls)
csse_covid_19_time_series <- lapply(urls, read.csv)

str(csse_covid_19_time_series[[1]])

## https://github.com/CSSEGISandData/COVID-19/tree/master/who_covid_19_situation_reports

### https://github.com/CSSEGISandData/COVID-19/tree/master/who_covid_19_situation_reports/who_covid_19_sit_rep_pdfs
### solo pdfs

### https://github.com/CSSEGISandData/COVID-19/tree/master/who_covid_19_situation_reports/who_covid_19_sit_rep_time_series
### csv, tutti gli stati, dal 1/21/2020 al 4/3/2020 (fine gennaio-inizio aprile)
