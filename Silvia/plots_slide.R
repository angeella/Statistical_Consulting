

#plot PCA
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

#pca la faccio su tutti gli stati e poi scremo dopo

dat <- #list(countries=
  COVID19::covid19(level=1,
                   #ISO=c("ITA", "ESP", "FRA", "GBR","CHN", "IRL", "GRC", "ROU", 
                   #"RUS", "AUT", "PER", "ECU", "SAU", "NLD", "BEL", "USA", "TUR", 
                   #"IRN", "DNK", "FIN", "NOR", "PRT", "CAN", "CHE", "BRA", "SWE", "SGP", "KOR", "DEU"),
                   start="2020-01-01", end="2020-05-14") %>% as.data.frame()

ISO=c("ITA", "ESP", "FRA", "GBR", "IRL", "GRC",  
      "AUT", "NLD", "BEL", "USA", 
      "DNK", "FIN", "NOR", "PRT", "CAN", "CHE", "SWE", "SGP", "KOR", "DEU")


#dat <- merger(dat)

policies=names(dat)[c(10:20)]


####### polychoric PCA for POLICIES ordinal variables

require(psych)

W <- polychoric(dat[,policies])
W$rho
p3 <- principal(r = W$rho, nfactors = 6) 
p3$loadings #prime 3 componenti
datapca=as.data.frame(p3$loadings[,c(1:3)])
datapca$policies=rownames(datapca)
datapca$coding=as.factor(c(rep("R", 8), rep("A", 3)))
rownames(datapca) <- NULL

dpca=gather(datapca, "PC", "Loadings", 1:3)

### plot
require(ggplot2)
library(RColorBrewer)

mypal <- colorRampPalette(brewer.pal(6, "PuBu"))
mypal2 <- colorRampPalette(brewer.pal(6, "YlOrRd"))

dpca$PC=as.factor(dpca$PC)
dpca = dpca %>% 
  group_by(PC) %>% 
  mutate(position = rank(-Loadings))


# Barplot
ggplot(dpca, aes(x=PC, y=Loadings, fill=policies, group = position)) + 
  geom_bar(stat = "identity", position="dodge", aes(color=coding)) +
  coord_flip()#+scale_fill_brewer(palette="Paired")