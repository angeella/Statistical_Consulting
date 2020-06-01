
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
source("Angela/merge_dat_R0.R")
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

dat <- add_R0(dataset = dat)

summary(dat$R0mean)

#drop PSE, GUY
#Plot R0 over time by country

dat1 <- dat %>% filter(id %in% c(unique(id)[1:54]))

lattice::xyplot(R0mean ~ date| id, groups = id, data=dat1,type=c('p','r'), auto.key=F)

dat1 <- dat %>% filter(id %in% c(unique(id)[55:114]))

lattice::xyplot(R0mean ~ date| id, groups = id, data=dat1,type=c('p','r'), auto.key=F)

dat1 <- dat %>% filter(id %in% c(unique(id)[115:190]))

lattice::xyplot(R0mean ~ date| id, groups = id, data=dat1,type=c('p','r'), auto.key=F)

#Italy, Spain, France, UK, China
dat1 <- dat %>% filter(id %in% c("ITA", "ESP", "GBR", "FRA"))

ggplot(data=dat1, aes(x=date, y=R0mean,fill = id, col = id)) +geom_line()
  
#Ned, Belgium, USA
dat1 <- dat %>% filter(id %in% c("BRA","USA", "BEL", "NLD", "SWE", "FIN"))

ggplot(data=dat1, aes(x=date, y=log(R0mean),fill = id, col = id)) +geom_line()

#Ned, Belgium, USA
dat1 <- dat %>% filter(id %in% c("BRA"))

ggplot(data=dat1, aes(x=date, y=R0mean,fill = id, col = id)) +geom_line()

ggplot(data=dat1, aes(x=date, y=confirmed,fill = id, col = id)) +geom_line()

dat1$id[dat1$R0mean <0]
summary(dat1$R0mean)
