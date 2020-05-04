require(COVID19)
require(ggplot2)
require(dplyr)
require(countrycode)
require(ISOcodes)
require(stringr)
require(stringi)
require(stringdist)
require(maps)
require(rnaturalearth)
require(rnaturalearthdata)

library(dygraphs)
library(xts)
library(EpiDynamics) #non me lo fa installareeee
library(webshot)
library(bsts)
library(ggplot2)
library(knitr)
library(splines)
library(RSQLite)

# librerie
source("Michele/lib/long2wide.R") # per convertire il dataset default dal long al wide format
source("Michele/lib/merger.R") # per fare il join con altri dataset
source("Michele/lib/policies.R") # contiene le codifiche delle politiche del dataset COVID19::covid19()
source("Michele/lib/lagdata.R") # contiene le codifiche delle politiche del dataset COVID19::covid19()


dat <- #list(countries=
  COVID19::covid19(level=1,
                   #ISO=c("ITA", "ESP", "FRA", "GBR","CHN", "IRL", "GRC", "ROU", 
                   #"RUS", "AUT", "PER", "ECU", "SAU", "NLD", "BEL", "USA", "TUR", 
                   # "IRN", "DNK", "FIN", "NOR", "PRT", "CAN", "CHE", "BRA", "SWE", "SGP", "KOR")
                   #,
                   start="2020-01-01") %>% as.data.frame()

# classificazione preliminare 
# HARD lockdown (Italy, Spain, France, UK, China,Irlanda, Grecia, Russia, Austria, Peru, Ecuador, South Arabia) 
# SOFT lockdown (Ned, Belgium, USA, Turkey**, Iran**, Denmark, Finland, Norway, Portugal, Canada, Switzerland)
# Nothing (Brazil, Sweden)
#politiche di contenimento molto particolari: Singapore, S. Korea


dat <- merger(dat)
head(dat)

for (p in policies) {
  dat[[p]] <- ordered(as.character(dat[[p]]), levels=names(policies.levels[[p]]))
}

#View(dat)

#reshape data (lo faccio su variabili individuali cosi mi viene piu comodo gestirmele dopo)

data=dat[,c(1,2,4)] # intanto uso i confirmed poi userò l'R0
library(tidyr)
CC=spread(data = data,
               key = date,
               value = confirmed)
dim(CC)


# stringency index


data=dat[,c(1,2,21)] # intanto uso i confirmed poi userò l'R0
SI=spread(data = dat[,c(1,2,21)],
          key = date,
          value = stringency_index)


# R0 come trovarlo 
# https://web.stanford.edu/~jhj1/teachingdocs/Jones-on-R0.pdf
# https://github.com/Paolin83/COVID19_SEIR_model/blob/master/draft_analysis_Veneto_new.md


##############------------------FUNCTIONAL ANALYSIS

# creating functional data: 191 countries and 101 time points (knots)
library(fda)
library(funHDDC)
library(fda.usc)
library(funFEM)

#### 
CC=as.matrix(CC[,-1])

ncfund=fdata(CC)
plot.fdata(ncfund)
optbasis=optim.basis(ncfund, #cerca il numero di basi ottime via cross-validation
                     type.CV = GCV.S,
                     lambda = 0,
                     numbasis = floor(seq(ncol(ncfund)/16, ncol(ncfund)/2, len = 10)),
                     type.basis = "bspline")$numbasis.opt

t =1:(dim(CC)[2])
basis = create.bspline.basis(c(0,dim(CC)[2]), norder = 4, nbasis=46) # order = degree+1
CCsm=smooth.basis(y=t(CC), argvals=t, basis)$fd
plot(CCsm, lty=1, col="black") 


##
SI=as.matrix(SI[,-1])
ncfund=fdata(SI)
plot.fdata(ncfund)
optbasis=optim.basis(ncfund, #cerca il numero di basi ottime via cross-validation
                     type.CV = GCV.S,
                     lambda = 0,
                     numbasis = floor(seq(ncol(ncfund)/16, ncol(ncfund)/2, len = 10)),
                     type.basis = "bspline")$numbasis.opt

t =1:(dim(SI)[2])
basis = create.bspline.basis(c(0,dim(SI)[2]), norder = 4, nbasis=46) # order = degree+1
SIsm=smooth.basis(y=t(SI), argvals=t, basis)$fd
plot(SIsm, lty=1, col="black") 

##

require(funHDDC)
res2 <- funHDDC(list(SIsm, CCsm), model = c('AkjBkQkDk', 'AkjBQkDk', 'AkBkQkDk', 'ABkQkDk', 'AkBQkDk', 'ABQkDk'),
                K = 1:5) 



