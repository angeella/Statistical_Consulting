
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

# librerie
source("Michele/lib/long2wide.R") # per convertire il dataset default dal long al wide format
source("Michele/lib/merger.R") # per fare il join con altri dataset
source("Michele/lib/policies.R") # contiene le codifiche delle politiche del dataset COVID19::covid19()
source("Angela/Compute_R0.R")

dat <- #list(countries=
  COVID19::covid19(level=1,
                   ISO=c("ITA", "ESP", "FRA", "GBR","CHN", "IRL", "GRC", "ROU", 
                   "RUS", "AUT", "PER", "ECU", "SAU", "NLD", "BEL", "USA", "TUR", 
                    "IRN", "DNK", "FIN", "NOR", "PRT", "CAN", "CHE", "BRA", "SWE", "SGP", "KOR")
                   ,
                   start="2020-01-01") %>% as.data.frame()

ISO=c("ITA", "ESP", "FRA", "GBR","CHN", "IRL", "GRC", "ROU", 
"RUS", "AUT", "PER", "ECU", "SAU", "NLD", "BEL", "USA", "TUR", 
"IRN", "DNK", "FIN", "NOR", "PRT", "CAN", "CHE", "BRA", "SWE", "SGP", "KOR")


dat <- merger(dat)

for (p in policies) {
  dat[[p]] <- ordered(as.character(dat[[p]]), levels=names(policies.levels[[p]]))
}
dat<- add_R0(dataset = dat)

#ecopolicies=names(dat)[c(78,81,83,85)] #prime due ordinali, seconde numeriche
#healthpolicies=names(dat)[c(94,96)] #numeriche

#considero tutte insieme le variabili ordinali
#policies=c(policies, names(dat)[c(78,81)] )

####### polychoric PCA for POLICIES ordinal variables

require(psych)


W <- polychoric(dat[,policies])
W$rho
p3 <- principal(r = W$rho, nfactors = 6) 
p3$loadings #prime 3 componenti
cbind(policies, p3$loadings)
#1a restrictions pol 
#2a contact tracing +info+testing 
#3a eco policies

p3$scores <- factor.scores(sapply(dat[,policies], as.numeric),p3)
PCpolicies=p3$scores$scores[,1:3]
colnames(PCpolicies)=c("restrinctions", "tracing", "testing")


########## FUNCTIONAL ANALYSIS

dat=cbind(dat, PCpolicies)

#reshape data 
library(tidyr)
# smoothing other economic and health var
source("Silvia/reshapami.R")
resdat=reshapami(dat)

rs=resdat$restrinctions
test=resdat$testing
tracing=resdat$tracing

# fd

library(fda)
library(funHDDC)
library(fda.usc)
library(funFEM)

source("Silvia/smoothing.R")

# SMOOTHING RESTRICTIONS
resSM=smoothing(rs)
res <- funFEM(resSM, model = "all", K = 2:6)
res$K 
fdmeans <- resSM
fdmeans$coefs <- t(res$prms$my)
plot(fdmeans, col=1:max(res$cls), lwd=2)

# SMOOTHING TESTING
testSM=smoothing(test)
res <- funFEM(testSM, model = "all", K = 2:6)
res$K
fdmeans <- testSM
fdmeans$coefs <- t(res$prms$my)
plot(fdmeans, col=1:max(res$cls), lwd=2)

test[res$cls==4,1]


# SMOOTHING TRACING
traceSM=smoothing(tracing)
res <- funFEM(traceSM, model = "all", K = 2:6)
res$K
fdmeans <- testSM
fdmeans$coefs <- t(res$prms$my)
plot(fdmeans, col=1:max(res$cls), lwd=2)

test[res$cls==1,1]


# smoothing other economic and health var

# troppi na nel mondo, nel nostro caso sostituisco i NA con zeri (ma boh)

fiscalm=resdat$ox.E3_Fiscal.measures
intsupport=resdat$ox.E4_International.support
invhealth=resdat$ox.H4_Emergency.investment.in.healthcare
invax=resdat$ox.H5_Investment.in.vaccines

#
fiscalm[is.na(fiscalm)] <- 0
intsupport[is.na(intsupport)] <- 0
invhealth[is.na(invhealth)] <- 0
invax[is.na(invax)] <- 0

fm=smoothing(fiscalm)
is=smoothing(intsupport)
ih=smoothing(invhealth)
iv=smoothing(invax)

#### clustering using 3 pc of ordinal variables and 4 numeric variables


