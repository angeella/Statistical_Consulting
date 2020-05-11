
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
#pca la faccio su tutti gli stati e poi scremo dopo

dat <- #list(countries=
  COVID19::covid19(level=1,
                   #ISO=c("ITA", "ESP", "FRA", "GBR","CHN", "IRL", "GRC", "ROU", 
                   #"RUS", "AUT", "PER", "ECU", "SAU", "NLD", "BEL", "USA", "TUR", 
                    #"IRN", "DNK", "FIN", "NOR", "PRT", "CAN", "CHE", "BRA", "SWE", "SGP", "KOR"),
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
dat=dat[dat$id %in% ISO,]

#reshape data 
library(tidyr)
# smoothing other economic and health var
source("Silvia/reshapami.R")

#creo var cumulate delle politiche eco e sanitarie

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
res=smoothing(rs)
resCL <- funFEM(res, model = "all", K = 2:6)
resCL$K 
fdmeans <- res
fdmeans$coefs <- t(resCL$prms$my)
plot(fdmeans, col=1:max(resCL$cls), lwd=2)

# SMOOTHING TESTING
tes=smoothing(test)
testCL <- funFEM(tes, model = "all", K = 2:6)
testCL$K
fdmeans <- tes
fdmeans$coefs <- t(testCL$prms$my)
plot(fdmeans, col=1:max(testCL$cls), lwd=2)

test[res$cls==4,1]


# SMOOTHING TRACING
tra=smoothing(tracing)
traCL <- funFEM(tra, model = "all", K = 2:6)
traCL$K
fdmeans <- tra
fdmeans$coefs <- t(traCL$prms$my)
plot(fdmeans, col=1:max(traCL$cls), lwd=2)

test[res$cls==1,1]


# smoothing other economic and health var

# troppi na nel mondo, nel nostro caso sostituisco i NA con zeri (ma boh)

fiscalm=resdat$ox.E3_Fiscal.measures[,-1]
intsupport=resdat$ox.E4_International.support[,-1]
invhealth=resdat$ox.H4_Emergency.investment.in.healthcare[,-1]
invax=resdat$ox.H5_Investment.in.vaccines[,-1]


#
fiscalm[is.na(fiscalm)] <- 0
intsupport[is.na(intsupport)] <- 0
invhealth[is.na(invhealth)] <- 0
invax[is.na(invax)] <- 0

fiscalm=t(apply(fiscalm, 1, cumsum))
intsupport=t(apply(intsupport, 1, cumsum))
invhealth=t(apply(invhealth, 1, cumsum))
invax=t(apply(invax, 1, cumsum))

fm=smoothing(fiscalm)
is=smoothing(intsupport) 
ih=smoothing(invhealth)
iv=smoothing(invax) 



# non mi funziona Funclustering

#provo con funHDDC
# funziona apparentemente solo su univariato
require(funHDDC)
res2 <- funHDDC(tes, model = c('AkjBkQkDk', 'AkjBQkDk', 'AkBkQkDk', 'ABkQkDk', 'AkBQkDk', 'ABQkDk'),
                K = 1:5) 
# tracing: 3 cluster
# restrictions: 3 cluster
# tests: 


# coclustering funLBM non funzionale

pol=abind(list(rs[,-1], tracing[,-1], test[,-1],fiscalm,intsupport,invax,invhealth), along=3)



pol2=array(dim=c(27,7,107))
for (i in 1:dim(pol)[2]){
for (j in 1:dim(pol)[3]){
 pol2[,j,i]=pol[,i,j]
  }
}

#senza var eco e san
require(funLBM)
ciao=funLBM(pol2[,-c(4:7),], K=2:3, L=2)
clust=data.frame(id=ISO[-5], cluster=ciao$row_clust)
ciao$col_clust # chiaramente restrictions e tracing + testing sono diverse

uno=clust[clust$cluster==1,]$id
due=clust[clust$cluster==2,]$id
tre=clust[clust$cluster==3,]$id
#quattro=clust[clust$cluster==4,]$id
#clust[clust$cluster==5,]$id
#clust[clust$cluster==6,]$id

#clustering_da_salvare=ciao

par(mfrow=c(1,3)) #poche restrizioni personali, no tracing eccetto per SWE, no testing eccetto per USA
plot(fdata(as.matrix(rs[rs$id %in% uno,-1])), main="Restrictions")
plot(fdata(as.matrix(tracing[tracing$id %in% uno,-1])), main="Tracing")
plot(fdata(as.matrix(test[test$id %in% uno,-1])), main="Testing")

par(mfrow=c(1,3)) #molte restrizioni (very uniform in the restriction pattern), no testing, very discontinuos tracing
plot(fdata(as.matrix(rs[rs$id %in% due,-1])), main="Restrictions")
plot(fdata(as.matrix(tracing[tracing$id %in% due,-1])), main="Tracing")
plot(fdata(as.matrix(test[test$id %in% due,-1])), main="Testing")

par(mfrow=c(1,3)) #testing molto elevato, tracing only in the first fase
plot(fdata(as.matrix(rs[rs$id %in% tre,-1])), main="Restrictions")
plot(fdata(as.matrix(tracing[tracing$id %in% tre,-1])), main="Tracing")
plot(fdata(as.matrix(test[test$id %in% tre,-1])), main="Testing")

par(mfrow=c(1,3)) # restr+test no tracing
plot(fdata(as.matrix(rs[rs$id %in% quattro,-1])), main="Restrictions")
plot(fdata(as.matrix(tracing[tracing$id %in% quattro,-1])), main="Tracing")
plot(fdata(as.matrix(test[test$id %in% quattro,-1])), main="Testing")


#### ora lo faccio con le var eco/san cumulate (non funziona troppi zero)
require(funLBM)
ciao=funLBM(pol2[,-c(4,5,6,7),], K=2:3, L=2)
clust=data.frame(id=ISO[-5], cluster=ciao$row_clust)
ciao$col_clust # chiaramente restrictions e tracing + testing sono diverse


##### provo con altro pacchetto mixedClust 

#funziona solo se non hai più di 50 time points boh 
# prendo le medie mobili

#costruisco le medie mobili con n=3 dell'array originario e ri-adatto alla funzione
pol3=array(dim=c(27,107,7))
for (i in 1:7) pol3[,,i]=t(apply(pol[,,i], 1, function(x) movavg(x, 3, type="s")))

pol4=array(dim=c(27,7,107))
for (i in 1:dim(pol3)[2]){
  for (j in 1:dim(pol3)[3]){
    pol4[,j,i]=pol3[,i,j]
  }
}

require(mixedClust)

res=mixedCoclust(distrib_names = c("Functional"),kr = 3, kc = c(3),
                    init = "random", nbSEM = 120, nbSEMburn = 100,
                  nbindmini = 1, functionalData=pol4[,-5,seq(9,107, 2)])

res@zc
res@zr
clust=data.frame(id=ISO[-5], cluster=res@zr)

uno=clust[clust$cluster==1,]$id
due=clust[clust$cluster==2,]$id
tre=clust[clust$cluster==3,]$id

par(mfrow=c(2,3)) #poche restrizioni personali, no tracing eccetto per SWE, no testing eccetto per USA
plot(fdata(as.matrix(rs[rs$id %in% uno,-1])), main="Restrictions", ylim=c(-2,4))
plot(fdata(as.matrix(tracing[tracing$id %in% uno,-1])), main="Tracing", ylim=c(-2,4))
plot(fdata(as.matrix(test[test$id %in% uno,-1])), main="Testing", ylim=c(-2,4))
plot(fdata(as.matrix(fiscalm[test$id %in% uno,])), main="FiscalM")
plot(fdata(as.matrix(invax[test$id %in% uno,])), main="InVax")
plot(fdata(as.matrix(invhealth[test$id %in% uno,])), main="InvHealth")


par(mfrow=c(1,3)) #molte restrizioni (very uniform in the restriction pattern), no testing, very discontinuos tracing
plot(fdata(as.matrix(rs[rs$id %in% due,-1])), main="Restrictions", ylim=c(-2,4))
plot(fdata(as.matrix(tracing[tracing$id %in% due,-1])), main="Tracing", ylim=c(-2,4))
plot(fdata(as.matrix(test[test$id %in% due,-1])), main="Testing", ylim=c(-2,4))
plot(fdata(as.matrix(fiscalm[test$id %in% due,])), main="FiscalM")
plot(fdata(as.matrix(invax[test$id %in% due,])), main="InVax")
plot(fdata(as.matrix(invhealth[test$id %in% due,])), main="InvHealth")

par(mfrow=c(1,3)) #testing molto elevato, tracing only in the first fase
plot(fdata(as.matrix(rs[rs$id %in% tre,-1])), main="Restrictions", ylim=c(-2,4))
plot(fdata(as.matrix(tracing[tracing$id %in% tre,-1])), main="Tracing", ylim=c(-2,4))
plot(fdata(as.matrix(test[test$id %in% tre,-1])), main="Testing", ylim=c(-2,4))
plot(fdata(as.matrix(fiscalm[test$id %in% tre,])), main="FiscalM")
plot(fdata(as.matrix(invax[test$id %in% tre,])), main="InVax")
plot(fdata(as.matrix(invhealth[test$id %in% tre,])), main="InvHealth")

require(pracma)
movavg(ciao, 3, type="s")
ciao=c(1,2,3,4)
