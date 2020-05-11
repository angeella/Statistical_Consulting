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
cumul <- c("deaths", "confirmed", "tests", "recovered")
instant <- c("hosp", "icu", "vent")
demo <- colnames(dat)[grepl("^pop", colnames(dat))]
geo <- c("country", "state", "city", "lat", "lng")
index <- "stringency_index"
id <- "id"
time <- "date"

isTRUE(length(setdiff(policies, colnames(dat)))==0)

for (p in policies) {
  dat[[p]] <- ordered(as.character(dat[[p]]), levels=names(policies.levels[[p]]))
}

#View(dat)


ggplot(data=dat, aes(x=date, y=confirmed, colour=id)) +
  geom_line() +
  geom_point()+ theme(legend.position= "none") 

#reshape data (lo faccio su variabili individuali cosi mi viene piu comodo gestirmele dopo)

data=dat[,c(1,2,4)] #confirmed cases
library(tidyr)
rs_data=spread(data = data,
       key = date,
       value = confirmed)
dim(rs_data)
str(rs_data)

confirmed=as.matrix(rs_data[,-1])
newcases=t(apply(confirmed, 1, function(x) diff(x)))

newcases=newcases/confirmed[,-1]
newcases[is.nan(newcases)]= 0

rownames(newcases)=rs_data[,1]
colnames(newcases)=names(rs_data)[-c(1,2)]

SI=spread(data = dat[,c(1,2,21)],
               key = date,
               value = stringency_index)



##############------------------FUNCTIONAL ANALYSIS

# creating functional data: 191 countries and 101 time points (knots)
library(fda)
library(funHDDC)
library(fda.usc)
library(funFEM)

ncfund=fdata(newcases)
plot.fdata(ncfund)
optbasis=optim.basis(ncfund, #cerca il numero di basi ottime via cross-validation
  type.CV = GCV.S,
  lambda = 0,
  numbasis = floor(seq(ncol(ncfund)/16, ncol(ncfund)/2, len = 10)),
  type.basis = "bspline")$numbasis.opt

t =1:101
basis = create.bspline.basis(c(0,101), norder = 4, nbasis=optbasis) # order = degree+1
SMbasis=smooth.basis(y=t(newcases), argvals=t, basis)$fd
plot(SMbasis, lty=1, col="black") 

# clustering via funFEM (probabilistic clustering)

ciao=funHDDC(SMbasis, model=c("AkjBkQkDk", "AkjBQkDk", "AkBkQkDk", "ABkQkDk", "AkBQkDk", "ABQkDk", "AkjBkQkDk"), K=1:6)
res=funFEM(SMbasis, model = "all", K = 2:6)
fdmeans = SMbasis; fdmeans$coefs = t(res$prms$my)
plot(fdmeans,col=1:res$K,xaxt='n',lwd=2) 
# boh non è interpretabile e di per se la variabile fa molto rumore, userei 
# altro, tipo l'R0 etc.

##### lo faccio univariato con lo Stringency index

dati <- as.matrix(SI[,-1])
plot(dati[1,], type = "n", ylim = c(min(dati)-0.1, max(dati)+0.1))
for (i in 1:nrow(dati)) {
  lines(dati[i,], lty = 2)
}
require(funFEM)

ncfund=fdata(dati)
plot.fdata(ncfund)
optbasis=optim.basis(ncfund, #cerca il numero di basi ottime via cross-validation
                     type.CV = GCV.S,
                     lambda = 0,
                     numbasis = floor(seq(ncol(ncfund)/16, ncol(ncfund)/2, len = 10)),
                     type.basis = "bspline")$numbasis.opt

basis <- create.bspline.basis(c(0,102), nbasis = optbasis, norder = 4)
fdobj <- smooth.basis(1:102, t(dati), basis)$fd
plot(fdobj)

res <- funFEM(fdobj, model = "all", K = 2:6)
res$K
# di base sto pacchetto seleziona solo due cluster all'inizio (quelli che hanno fatto qualcosa
# /quelli che non hanno fatto nulla), togliendo l'ultimo cluster e rerunnandolo 
# mi seleziona 4 cluster diversi, di base le misture rimangono significative solo su due
# noi sceglieremo un modello con 3/4 cluster in ogni caso direi

SI[res$cls==1,1]
SI[res$cls==2,1]
SI[res$cls==3,1]
SI[res$cls==4,1] # da plottare su mappe

fdmeans = fdobj; fdmeans$coefs = t(res$prms$my)
plot(fdmeans,col=1:res$K,xaxt='n',lwd=2)

plot(dati[1,], type = "n", ylim = c(min(dati)-0.1, max(dati)+0.1))
for (i in 1:nrow(dati)) {
  lines(dati[i,], lty = 4, lwd = 0.5, col = res$cls[i])
}

#######K MEAN FUNZIONALE 

#divertitevi a guardare come cambia
a <- kmeans.fd(fdobj, ncl = 4)
a <- kmeans.fd(fdobj, ncl = 3)
a <- kmeans.fd(fdobj, ncl = 3)
a <- kmeans.fd(fdobj, ncl = 3)
a <- kmeans.fd(fdobj, ncl = 3)
a <- kmeans.fd(fdobj, ncl = 3)
a <- kmeans.fd(fdobj, ncl = 3)
a <- kmeans.fd(fdobj, ncl = 3)
a <- kmeans.fd(fdobj, ncl = 3)
a <- kmeans.fd(fdobj, ncl = 3)
a <- kmeans.fd(fdobj, ncl = 3)
 
########################################## usando il pacchetto funHDDC (
# sempre clustering prob ma modella ogni cluster nel suo specifico sottospazio funzionale

require(funHDDC)
res2 <- funHDDC(fdobj, model = c('AkjBkQkDk', 'AkjBQkDk', 'AkBkQkDk', 'ABkQkDk', 'AkBQkDk', 'ABQkDk'),
                K = 1:5) # ci mette un sacco

# statisticamente non ci sono differenze significative tra le curve per cui K=1
# comunque il secondo modello migliore è K=4, che direi che per noi è perfetto


