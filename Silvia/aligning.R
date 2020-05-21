

# alignment at the day of the first contagion (from 10 days before)



# missing values treatment via spline approx

missval=function(reshline){
  z=zoo(reshline)
  z=na.approx(z, 1:85)
  return(z)
}

# aligning, cutting and na replacement function

aligning=function(reshaped, fcdays=firstc){
 
   numdb=as.matrix(reshaped[,-1])
   alignedc=matrix(nrow=dim(numdb)[1],ncol=24+dim(numdb)[2])
   
   for (i in 1:dim(numdb)[1]){
       if(fcdays[i]>25) alignedc[i,c(1:(dim(numdb)[2]-fcdays[i]+1+24))]=numdb[i, c((fcdays[i]-24):dim(numdb)[2])]
       else alignedc[i,c((25+1-fcdays[i]):(114+25-fcdays[i]))]=numdb[i,]
     }
    alignedc=alignedc[,c(15:99)] #80 righe (5 giorni prima del contagio + 75 dopo contagio)
   # finalmat=t(apply(alignedc, 1, function(x) missval(x)))
    
  return(alignedc)}




