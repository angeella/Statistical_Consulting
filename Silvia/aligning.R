

# alignment at the day of the first contagion (from 10 days before)

require(zoo)

resdat=reshapami(dat)
seldb=resdat$confirmed



#alignment of the wide format
# facciamo al 25 esimo giorno

firstc=apply(seldb[,-1], 1, function(x) min(which(x!=0)))

# missing values treatment via spline approx

missval=function(reshline){
  z=zoo(reshline)
  z=na.spline(z)
  return(z)
}

# aligning, cutting and na replacement function

alignining=function(reshaped, fcdays=firstc){
  
   numdb=as.matrix(reshaped[,-1])
   alignedc=matrix(nrow=28,ncol=138)
   for (i in 1:28){
       if(fcdays[i]>25) alignedc[i,c(1:(114-fcdays[i]+25))]=numdb[i, c((fcdays[i]-24):114)]
       else alignedc[i,c((25+1-fcdays[i]):(114+25-fcdays[i]))]=numdb[i,]
     }
    alignedc=alignedc[,c(20:99)] #80 righe (5 giorni prima del contagio + 75 dopo contagio)
    finalmat=t(apply(alignedc, 1, function(x) missval(x)))
    
  return(finalmat)}

contagi=alignining(seldb)


