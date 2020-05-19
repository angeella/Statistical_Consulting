

dat_shape <- dat %>%
  left_join(dat %>%
              filter(confirmed >= 30) %>%
              group_by(id) %>%
              summarise(date.start=min(date)),
            by="id") %>%
  mutate(date2=date-date.start) %>%
  as.data.frame()

p <- dat_shape %>% ggplot(aes(x=date2, y=R0, group=Clusters, color=Clusters)) +
  geom_smooth() 

ggplotly(p)


p <- dat_shape %>% ggplot(aes(x=date, y=R0, group=Clusters, color=Clusters)) +
  geom_smooth() 

ggplotly(p)


aligning=function(reshaped){
  fcdays=apply(seldb[,-1], 1, function(x) min(which(x!=0)))
  
  numdb=as.matrix(reshaped[,-1])
  alignedc=matrix(nrow=28,ncol=138)
  for (i in 1:28){
    if(fcdays[i]>25) alignedc[i,c(1:(114-fcdays[i]+25))]=numdb[i, c((fcdays[i]-24):114)]
    else alignedc[i,c((25+1-fcdays[i]):(114+25-fcdays[i]))]=numdb[i,]
  }
  alignedc=alignedc[,c(15:99)] #80 righe (5 giorni prima del contagio + 75 dopo contagio)
  # finalmat=t(apply(alignedc, 1, function(x) missval(x)))
  
  return(alignedc)}