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
                   #"IRN", "DNK", "FIN", "NOR", "PRT", "CAN", "CHE", "BRA", "SWE", "SGP", "KOR", "DEU"),
                   start="2020-01-01") %>% as.data.frame()

ISO=c("ITA", "ESP", "FRA", "GBR","CHN", "IRL", "GRC", "ROU", 
      "RUS", "AUT", "SAU", "NLD", "BEL", "USA", "TUR", 
      "IRN", "DNK", "FIN", "NOR", "PRT", "CAN", "CHE", "SWE", "SGP", "KOR", "DEU")


#dat <- merger(dat)

policies=names(dat)[c(10:20)]


####### polychoric PCA for POLICIES ordinal variables

require(psych)

W <- polychoric(dat[,policies])
W$rho
p3 <- principal(r = W$rho, nfactors = 6) 
p3$loadings #prime 3 componenti
cbind(policies, p3$loadings)
distancematrix=as.dist(1-W$rho)^2

centered=scale(dat[,policies], TRUE, FALSE)
PCpolicies <- as.matrix(centered) %*% p3$weights
PCpolicies=PCpolicies[,c(1:3)]

#dat$restrictions=PCpolicies[,1]
#p3$scores <- factor.scores(sapply(dat[,policies], as.numeric),p3)
#PCpolicies=p3$scores$scores[,1:3]
colnames(PCpolicies)=c("restrictions", "tracing", "testing")


#clusters <- apply(PCpolicies[,c(1:3)], 1, function(x) which.max(x))

#c=data.frame(id=dat$id,clust=clusters)
#dataclust=aggregate(clusters, list(id=c$id), mean)

#dataclust$x=round(dataclust$x)

#dataclust[dataclust$id %in% ISO,]

########## FUNCTIONAL ANALYSIS

dat=cbind(dat, PCpolicies)
dat=dat[dat$id %in% ISO,] #dim 3192   43

#reshape data 
library(tidyr)
# smoothing other economic and health var
source("Silvia/reshapami.R")

#creo var cumulate delle politiche eco e sanitarie

resdat=reshapami(dat)

rs=resdat$restrictions
test=resdat$testing
tracing=resdat$tracing


##########  alignment at the first day of contagion
# cutting the time series leaving 5 days before contagion and 75 after

require(zoo)
seldb=resdat$confirmed

#alignment of the wide format
# facciamo al quinto giorno

firstc=apply(seldb[,-1], 1, function(x) min(which(x!=0)))

source("Silvia/aligning.R")

rs=aligning(rs, firstc)
test=aligning(test)
tracing=aligning(tracing)


#imputo i valori mancanti agli estremi gli ultimi esistenti sulle code
for(i in 1:dim(rs)[1]){
  if(sum(is.na(rs[i,c(1:40)]))!=0){
    rs[i,which(is.na(rs[i,c(1:40)]))]=rs[i,max(which(is.na(rs[i,c(1:40)])))+1]
    test[i,which(is.na(test[i,c(1:40)]))]=test[i,max(which(is.na(test[i,c(1:40)])))+1]
    tracing[i,which(is.na(tracing[i,c(1:40)]))]=tracing[i,max(which(is.na(tracing[i,c(1:40)])))+1]
  }
  if (sum(is.na(rs[i,]))!=0){
    rs[i,which(is.na(rs[i,]))]=rs[i,min(which(is.na(rs[i,])))-1]
    test[i,which(is.na(test[i,]))]=test[i,min(which(is.na(test[i,])))-1]
    tracing[i,which(is.na(tracing[i,]))]=tracing[i,min(which(is.na(tracing[i,])))-1]
  }
}



###################### FDA// solo sulle 3 componenti principali
library(fda)
library(funHDDC)
library(fda.usc)
library(funFEM)

source("Silvia/smoothing.R")

res=smoothing(rs)
tes=smoothing(test)
tra=smoothing(tracing)


require(funHDDC)
res2 <- funHDDC(list(tra,res,tes), model = c('AkjBkQkDk', 'AkjBQkDk', 'AkBkQkDk', 'ABkQkDk', 'AkBQkDk', 'ABQkDk'),
                K = 1:5) 

# coclustering funLBM
disturb=rnorm(2380, 0, 0.1)
max(disturb)

disturb=matrix(disturb, nrow=28)
distracing=tracing+disturb
distest=test+disturb

require(abind)
pol=abind(list(rs, tracing, test), along=3)


pol2=array(dim=c(25,3,dim(pol)[2]))
for (i in 1:dim(pol)[2]){
  for (j in 1:dim(pol)[3]){
    pol2[,j,i]=pol[,i,j]
  }
}

require(funLBM)
ciao=funLBM(pol2, K=2:5, L=2)

paesi=resdat$restrictions[,1]

clust=data.frame(id=paesi, cluster=ciao$row_clust)
ciao$col_clust


uno=clust[clust$cluster==1,]$id
due=clust[clust$cluster==2,]$id
tre=clust[clust$cluster==3,]$id
quattro=clust[clust$cluster==4,]$id
cinque=clust[clust$cluster==5,]$id



#plotting
library(tidyr)
library(ggplot2)

df=as.data.frame(rs)
df$id=resdat$restrinctions[,1]


par(mfrow=c(1,3)) 
plot(fdata(rs[ciao$row_clust==1,]), main="Restriction-based", ylim=c(-2,3.5))
plot(fdata(tracing[ciao$row_clust==1,]), main="Tracing-based", ylim=c(-2,3.5))
plot(fdata(test[ciao$row_clust==1,]), main="Testing-based", ylim=c(-2,3.5))

par(mfrow=c(1,3))
plot(fdata(rs[ciao$row_clust %in% c(2,6),]), main="Restriction-based", ylim=c(-2,3.5))
plot(fdata(tracing[ciao$row_clust %in% c(2,6),]), main="Tracing-based", ylim=c(-2,3.5))
plot(fdata(test[ciao$row_clust %in% c(2,6),]), main="Testing-based", ylim=c(-2,3.5))

par(mfrow=c(1,3)) 
plot(fdata(rs[ciao$row_clust==3,]), main="Restriction-based", ylim=c(-2,3.5))
plot(fdata(tracing[ciao$row_clust==3,]), main="Tracing-based", ylim=c(-2,3.5))
plot(fdata(test[ciao$row_clust==3,]), main="Testing-based", ylim=c(-2,3.5))

par(mfrow=c(1,3))
plot(fdata(rs[ciao$row_clust==4,]), main="Restriction-based", ylim=c(-2,3.5))
plot(fdata(tracing[ciao$row_clust==4,]), main="Tracing-based", ylim=c(-2,3.5))
plot(fdata(test[ciao$row_clust==4,]), main="Testing-based", ylim=c(-2,3.5))


par(mfrow=c(1,3))
plot(fdata(rs[ciao$row_clust==5,]), main="Restriction-based", ylim=c(-2,3.5))
plot(fdata(tracing[ciao$row_clust==5,]), main="Tracing-based", ylim=c(-2,3.5))
plot(fdata(test[ciao$row_clust==5,]), main="Testing-based", ylim=c(-2,3.5))


par(mfrow=c(1,3))  
plot(fdata(rs[ciao$row_clust==6,]), main="Restriction-based", ylim=c(-2,3.5))
plot(fdata(tracing[ciao$row_clust==6,]), main="Tracing-based", ylim=c(-2,3.5))
plot(fdata(test[ciao$row_clust==6,]), main="Testing-based", ylim=c(-2,3.5))

#restrizioni
medieclust=t(sapply(1:5, function(i) mean(fdata(rs[ciao$row_clust==i,]))$data))
plot(fdata(medieclust))
# 1=nero, 2=rosso, 3=verde, 4=blu, 5=turchino

#testing
medieclust=t(sapply(1:5, function(i) mean(fdata(test[ciao$row_clust==i,]))$data))
plot(fdata(medieclust))

#tracing
medieclust=t(sapply(1:5, function(i) mean(fdata(tracing[ciao$row_clust==i,]))$data))
plot(fdata(medieclust))


## cluster uno: testing-based CAN, DEU, GBR, SWE, USA
## cluster due: restrinctions(strong)-testing(mild)-based IRL, IRN, ITA, NLD, TUR
## cluster tre: mild levels of all 3 : AUT BEL CHE DNK ESP FIN FRA NOR RUS
## cluster QUATTRO: testing and tracing based KOR and SGP
## cluster cinque: (mild) restriction and (mild) testing, GRC PRT ROU SAU




####### CLUSTERING UNIVARIATO

# SMOOTHING RESTRICTIONS
res=smoothing(rs)
resCL <- funFEM(res, model = "all", K = 2:6)
resCL$K 
fdmeans <- res
fdmeans$coefs <- t(resCL$prms$my)
plot(fdmeans, col=1:max(testCL$cls), lwd=2)

plot(fdmeans$coefs[,2], col=1, lwd=2, main="Restriction", type="l", ylim=c(-1,2.5))

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


