#Load data Github

COVIDsite <- https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports

Feb <- c(22:29)
Mar <- c(1:31)
Apr <- c(1:)


githubURL <- "https://github.com/thefactmachine/hex-binning-gis-data/raw/master/popDensity.RData"

load(url(githubURL))