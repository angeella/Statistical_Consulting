#Load data Github

COVIDrecoveredGlobal <- "https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
  
COVIDrecoveredUS <- "https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"

COVIDrecoveredGlobal <- "https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"


read.csv(url(COVIDrecoveredGlobal))
