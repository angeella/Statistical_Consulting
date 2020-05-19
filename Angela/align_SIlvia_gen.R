
source("Silvia/reshapami.R")

resdat=reshapami(dat)
seldb=resdat$confirmed

#alignment of the wide format
# facciamo al 25 esimo giorno

firstc=apply(seldb[,-1], 1, function(x) min(which(x!=0)))

# missing values treatment via spline approx

missval=function(reshline){
  z=zoo(reshline)
  z=na.approx(z, 1:85)
  return(z)
}

# aligning, cutting and na replacement function

aligning<-function(dat){
  source("Silvia/reshapami.R")
  resdat<- reshapami(dat)
  seldb<- resdat$confirmed
  fcdays<- apply(seldb[,-1], 1, function(x) min(which(x!=0)))
  nid <- dim(resdat[1]$deaths)[1]
  nt <- dim(resdat[1]$deaths)[2]
  alignedc <- list()
  for(i in 1:length(resdat)){
    alignedc[[i]] <- matrix(nrow=nid,ncol=nt) #matrice finale con righe id
  }
  for(j in 1:length(resdat)){
  for (i in 1:nid){
    if(fcdays[i]>25){
      alignedc[j][i,c(1:(104-fcdays[i]+25))]<- as.data.frame(resdat[j])[i, c((fcdays[i]-24+1):104)]}else{
        alignedc[j][i,c((25+1-fcdays[i]):(104+25-fcdays[i]))]<- resdat[j][i,]
      }
  }
  }
  alignedc=alignedc[,c(15:99)] #80 righe (5 giorni prima del contagio + 75 dopo contagio)
  # finalmat=t(apply(alignedc, 1, function(x) missval(x)))
  
  return(alignedc)}


dat_shape <- aligning(dat)