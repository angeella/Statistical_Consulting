
#Compute R0 and merge in the dataset.

require(COVID19)
require(ggplot2)
require(dplyr)
require(countrycode)
require(ISOcodes)
require(stringr)
require(stringi)
require(stringdist)

# per mappe...
require(maps)
require(rnaturalearth)
require(rnaturalearthdata)

# librerie
source("Michele/lib/long2wide.R") 
source("Michele/lib/merger.R")
source("Michele/lib/policies.R") 
source("Michele/lib/lagdata.R") 
source("Angela/Compute_R0.R")
dat <- COVID19::covid19(level=1) %>% as.data.frame()
cumul <- c("deaths", "confirmed", "tests", "recovered")
instant <- c("hosp", "icu", "vent")
demo <- colnames(dat)[grepl("^pop", colnames(dat))]
geo <- c("country", "state", "city", "lat", "lng")
index <- "stringency_index"
id <- "id"
time <- "date"

dat <- dat %>% lagdata(vars=c(cumul, policies, index), lag = 1, save.lag = F, save.var = T)

dat <- merger(dat)
