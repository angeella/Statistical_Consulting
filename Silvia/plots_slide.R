
rm(list = ls())
#plot PCA
require(COVID19)
require(ggplot2)
require(dplyr)
require(countrycode)
require(ggpubr)
require(gridExtra)
require(psych)
require(viridis)
require(maps)
require(rnaturalearth)
require(rnaturalearthdata)

load("cluster20paesi.Rdata")

datapca=as.data.frame(p3$loadings[,c(1:3)])
datapca$policies=rownames(datapca)
datapca$coding=as.factor(c(rep("R", 8), rep("A", 3)))
rownames(datapca) <- NULL

dpca=gather(datapca, "PC", "Loadings", 1:3)

### plot

dpca$PC=as.factor(dpca$PC)
dpca = dpca %>% 
  group_by(PC) %>% 
  mutate(position = rank(-Loadings))

rpol=unique(dpca[dpca$coding=="R",]$policies)
apol=unique(dpca[dpca$coding=="A",]$policies)
peach=viridis(begin=0.5, end=0.8, 3,option = "A")
gray=viridis(begin=0.3, end=0.8, 8,option = "D")


ggplot(dpca, aes(x=PC, y=Loadings, fill=policies, group=position))+
  geom_bar(stat="identity", position="dodge")+
  coord_flip()+scale_fill_manual("", breaks=c(apol, rpol), 
                           values=c(gray[1], peach[1], gray[2], peach[2], gray[3:6], peach[3], gray[7:8]))


###### PLOT CLUSTERING


tib=function(x){
     medieclust=t(sapply(1:5, function(i) mean(fdata(x[ciao$row_clust==i,]))$data))
     colnames(medieclust)=seq(1,85, 1)
     cl=c("C1", "C2", "C3", "C5", "C4")
     clust_df=gather(cbind(cl,as.data.frame(medieclust)), "Time", "Results", 2:86)
     #clust_df$Time=as.factor(clust_rs$Time)
     
     return(clust_df)}

clust_rs=tib(rs)
restrict=clust_rs %>%
  ggplot(aes(x = Time, y = Results, group=cl)) +
  geom_smooth(aes(color=cl), method = "loess",span = 0.3, se=FALSE)+
  geom_vline(xintercept = 10,color = "lightsteelblue3")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ylab("Restrictions")+ 
  theme(legend.position = "none") +
  scale_color_viridis(discrete=TRUE, option="A", direction=-1, end=0.9,labels =c('KOR,SGP', 'DEU,SWE', 'CAN,GRC,PRT,USA', 'AUT,BEL,CHE,DNK,FIN,FRA,NOR','ITA,ESP,GBR,IRL,NLD'))

clust_test=tib(test)
testt=clust_test %>%
  ggplot(aes(x = Time, y = Results, group=cl)) +
  geom_smooth(aes(color=cl), method = "loess",span = 0.3, se=FALSE)+
  geom_vline(xintercept = 10,color = "lightsteelblue3")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ylab("Tests")+ 
  theme(legend.position = "none") +
  scale_color_viridis(discrete=TRUE, option="A", direction=-1, end=0.9,labels =c('KOR,SGP', 'DEU,SWE', 'CAN,GRC,PRT,USA', 'AUT,BEL,CHE,DNK,FIN,FRA,NOR','ITA,ESP,GBR,IRL,NLD'))

clust_trace=tib(tracing)
trace=clust_trace %>%
  ggplot(aes(x = Time, y = Results, group=cl)) +
  geom_smooth(aes(color=cl), method = "loess",span = 0.3, se=FALSE)+
  geom_vline(xintercept = 10,color = "lightsteelblue3")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom")+ylab("Tracing")+
  scale_color_viridis(discrete=TRUE,  option="A", direction=-1, end=0.9,labels =c('KOR,SGP', 'DEU,SWE', 'CAN,GRC,PRT,USA', 'AUT,BEL,CHE,DNK,FIN,FRA,NOR','ITA,ESP,GBR,IRL,NLD'))
  
  
ggarrange(restrict, testt, trace, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")

### mappe


clust$id <- countrycode(clust$id, 'iso3c', 'country.name')
clust$id[20] <- "USA"
clust$id[10] <- "UK"

clust$cluster=c(4, 4, 3 ,4, 2, 4, 5, 4, 4, 5, 3, 5, 5, 1 ,5, 4, 3, 1, 2, 3)

#cambio ordine dei cluster dal più lasco al meno (4 diventa 5 e 5 diventa 4)
world <- map_data("world")

tojoin <- as.data.frame(matrix(
  nrow = length(table(world$region)),
  ncol = 2,
  NA,
  dimnames = list(names(table(world$region)), colnames(clust))
))

tojoin$id <- rownames(tojoin)
tojoin$id=as.character(tojoin$id)

library(dplyr)
all <- full_join(clust, tojoin, by="id")
all <- all[order(all$id), ]
all$region=all$id
mapbig <- inner_join(world, all, by = "region")


worldmap <- ggplot() + theme(
  panel.background = element_rect(fill = "lightcyan1",
                                  color = NA),
  panel.grid = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank()
)


#europe
europe <- worldmap + coord_fixed(xlim = c(-15, 35), ylim = c(35, 70), ratio = 1.5)

europe2 <- europe + geom_polygon(data = mapbig,
                                 aes(fill = cluster.x,
                                     x = long,
                                     y = lat,
                                     group = group),
                                     color = "grey70") +
  theme(text = element_text(size = 30),
        plot.title = element_text(face = "bold")) +
  scale_fill_viridis(option = "magma", begin=0.2, end=0.9,
direction = -1,
name = "",
 na.value = "grey80",
guide = guide_colorbar(
 barheight = unit(140, units = "mm"),
barwidth = unit(6, units = "mm")))+ theme(legend.position = "none") 

europe2

###  america

na <- worldmap + coord_fixed(xlim = c(-170, -50), ylim = c(-12, 100), ratio = 1.5)
na2=na + geom_polygon(data = mapbig,
                      aes(fill = as.factor(cluster.x),
                          x = long,
                          y = lat,
                          group = group),
                      color = "grey70") +
  theme(text = element_text(size = 30),
        plot.title = element_text(face = "bold")) +
  scale_fill_viridis_d(option = "magma",begin=0.2, end=0.9,
                     direction = -1,
                     name = "",
                     na.value = "grey80",
                     #guide = guide_colorbar(
                       #barheight = unit(140, units = "mm"),
                       #barwidth = unit(6, units = "mm"))
                     )+
  theme(legend.position = "none") 

#asia

kor <- worldmap + coord_fixed(xlim = c(122, 132), ylim = c(32, 39), ratio = 1.5)
kor2=kor+ geom_polygon(data = mapbig,
                      aes(fill = cluster.x,
                          x = long,
                          y = lat,
                          group = group),
                      color = "grey70") +
  theme(text = element_text(size = 30),
        plot.title = element_text(face = "bold")) +
  scale_fill_viridis(option = "magma",begin=0.2, end=0.9,
                     direction = -1,
                     name = "",
                     na.value = "grey80",
                     guide = guide_colorbar(
                       barheight = unit(140, units = "mm"),
                       barwidth = unit(6, units = "mm")))+ theme(legend.position = "none") 

sin <- worldmap + coord_fixed(xlim = c(102, 105), ylim = c(0.5, 2.5), ratio = 1.5)
sin2=sin+ geom_polygon(data = mapbig,
                       aes(fill = cluster.x,
                           x = long,
                           y = lat,
                           group = group),
                       color = "grey70") +
  theme(text = element_text(size = 30),
        plot.title = element_text(face = "bold")) +
  scale_fill_viridis(option = "magma",begin=0.2, end=0.9,
                     direction = -1,
                     name = "",
                     na.value = "grey80",
                     guide = guide_colorbar(
                       barheight = unit(140, units = "mm"),
                       barwidth = unit(6, units = "mm")))+ theme(legend.position = "none") 



grid.arrange(na2, europe2,                     
             arrangeGrob(kor2, sin2, nrow = 2, ncol=1), 
             ncol = 3, nrow=1, widths=c(3,4,2))    

save.image("SAVEPLOTS.Rdata")
