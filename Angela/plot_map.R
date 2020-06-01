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
source("~/GitHub/Statistical_Consulting/Angela/packages.R")
load("~/GitHub/Statistical_Consulting/Angela/model.RData")
### mappa

clust$id <- countrycode(clust$id, 'iso3c', 'country.name')
clust$id[20] <- "USA"
clust$id[10] <- "UK"
pr<-ggeffect(mod1, "Clusters")

clust$predicted <- ifelse(clust$cluster == "1", pr$predicted[1],
                          ifelse(clust$cluster == "2", pr$predicted[2],
                                 ifelse(clust$cluster == "3", pr$predicted[3],
                                        ifelse(clust$cluster == "4", pr$predicted[4], pr$predicted[5]))))
clust$predicted <- round(clust$predicted,2)
clust$predicted <- as.factor(clust$predicted)
world <- map_data("world")
world$id <- world$region
mapbig <- left_join(world, clust, by="id")


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
                                 aes(fill = predicted,
                                     x = long,
                                     y = lat,
                                     group = group),
                                 color = "grey70") +
  theme(text = element_text(size = 30),
        plot.title = element_text(face = "bold")) +
  scale_fill_viridis_d(option = "magma", begin=0.2, end=0.9,
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
                      aes(fill = predicted,
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
                       na.translate=FALSE
                       #guide = guide_colorbar(
                       #barheight = unit(140, units = "mm"),
                       #barwidth = unit(6, units = "mm"))
  )+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 6, face = "bold"),
        legend.title=element_text(size=6), 
        legend.text=element_text(size=6)) + guides(fill=guide_legend(nrow=5, byrow=TRUE,ncol = 2))

na2
#asia

kor <- worldmap + coord_fixed(xlim = c(122, 132), ylim = c(32, 39), ratio = 1.5)
kor2=kor+ geom_polygon(data = mapbig,
                       aes(fill = predicted,
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
                     guide = guide_colorbar(
                       barheight = unit(140, units = "mm"),
                       barwidth = unit(6, units = "mm")))+ theme(legend.position = "none") 

sin <- worldmap + coord_fixed(xlim = c(102, 105), ylim = c(0.5, 2.5), ratio = 1.5)
sin2=sin+ geom_polygon(data = mapbig,
                       aes(fill = predicted,
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
                     guide = guide_colorbar(
                       barheight = unit(140, units = "mm"),
                       barwidth = unit(6, units = "mm")))+ theme(legend.position = "none") 

save(na2,europe2,kor2,sin2, file = "Angela/Data/plot_map.RData")

eff1 <- ggeffect(mod1, terms = c("Clusters", "stay_home_restrictionsF"))
attr(eff1, "legend.title") <- ""
attr(eff1,"title") <- "Stay Home"
levels(eff1$group) <-  c("No measures", "Recommend", "Essential trips", "Minimal exceptions")

p1 <- plot(eff1) + ylab("Predicted counts of Active")+ scale_color_discrete() +
  theme_minimal(base_size = 12)  + theme(plot.title =element_text(),
                                         axis.title.x = element_blank(),
                                         legend.position = "None")

eff2 <- ggeffect(mod1, terms = c("Clusters", "testing_policyF"))
attr(eff2, "legend.title") <- ""
attr(eff2,"title") <- "Testing"
levels(eff2$group) <-  c("No measures", "Specific criteria", "Symptoms", "Open")

p2 <- plot(eff2)+ ylab("Predicted counts of Active")+ scale_color_discrete() +
  theme_minimal(base_size = 12)  + theme(plot.title =element_text(),
                                         axis.title.x = element_blank(),
                                         legend.position = "None")

eff3 <- ggeffect(mod1, terms = c("Clusters", "contact_tracingF"))
attr(eff3, "legend.title") <- ""
attr(eff3,"title") <- "Tracing"
levels(eff3$group) <-  c("No measures", "Limited", "Comprehensive")
a <- plot(eff3)
p3 <- plot(eff3) + ylab("Predicted counts of Active")+ scale_color_discrete() +
  theme_minimal(base_size = 12)  + theme(plot.title =element_text(),
                                         axis.title.x = element_blank(),
                                         legend.position = "None")

save(p1,p2,p3, file = "~/GitHub/Statistical_Consulting/Presentation/Data/plot_mod.RData")



