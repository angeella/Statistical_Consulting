
require(COVID19)
require(ggplot2)
require(dplyr)
source("Michele/lib_long2wide.R") # per convertire il dataset default dal long al wide format
source("Michele/lib_policies.R") # contiene le codifiche delle politiche del dataset COVID19::covid19()

dat <- as.data.frame(COVID19::covid19(level=1))


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

policies.pca <- prcomp(scale(sapply(dat[,policies], as.numeric))) # una PCA fatta coi piedi, che però rivela un po' dello stringency index
plot(policies.pca)
policies.pca$x <- cbind(as.data.frame(policies.pca$x), index=dat$stringency_index)
ggplot(policies.pca$x) + geom_point(aes(x=PC1, y=PC2, color=index)) + scale_color_gradient(low = "green", high = "red")
# a grandi linee lo stringency index è la prima PC delle politiche usate come quantitative
# oppure una combinazione lineare delle prime due PC (vedete quel gradiente diagonale anche voi?)

require(cluster)

# il wide format: mette in colonne separate le misure ripetute, sulla stessa riga per uno stesso individuo (o nazione nel nostro caso)
stringency.wide <- long2wide(dat, id, time, policies) # strin...gency in wide... format, ahah
rownames(stringency.wide) <- stringency.wide[,1]
stringency.wide <- stringency.wide[,-1]
head(stringency.wide)[,1:10]

d <- daisy(stringency.wide, metric = "gower") # calcola distanze con indicatrici per variabili qualitative
stringency.clusters <- lapply(c("ward.D", "ward.D2", "single", "complete"), function(m) hclust(d, method = m))
lapply(stringency.clusters, function(v) plot(as.dendrogram(v), type = "triangle", main=v$method, xlab=""))
chosen <- stringency.clusters[[2]] # uso il Ward

require(maps)
require(rnaturalearth)
require(rnaturalearthdata)

for (ncl in 2:10) { # per ogni scelta di #cluster=k
  # calcola la classificazione
  aux <- data.frame(id=rownames(stringency.wide), cluster=cutree(chosen, k=ncl))
  newenc <- aux %>% left_join(dat[,c(id,index)], by=id) %>% group_by(cluster) %>% summarise(stringency=mean(stringency_index)) %>% as.data.frame()
  aux$cluster <- ordered(order(newenc$stringency, decreasing = T)[aux$cluster])
  
  # stampa l'andamento medio dello stringency index nel tempo nei vari cluster
  aux2 <- aux %>%
    left_join(dat[,c(id,time,index)], by=id)
  require(boot)
  print(
    aux2 %>%
      ggplot() +
      geom_smooth(aes_string(x=time, y=index, color="cluster"), method = "loess") +
      xlim(range(dat$date)) #+
      # ggtitle(paste("# cluster =", ncl))
  )
  
  # mostra i cluster su mappa
  print(ne_countries(scale = "medium", returnclass = "sf") %>%
    left_join(aux, by=c("iso_a3"="id")) %>%
    ggplot() +
    geom_sf(aes(fill=cluster), lwd=0) +
    ggtitle(paste("# cluster =", ncl))
  )
}


dat$id <- factor(dat$id, levels = as.character(rownames(stringency.wide)[chosen$order]))

require(maps)
require(rnaturalearth)
require(rnaturalearthdata)

isTRUE(max(table(x=dat$id, y=dat$date)) == 1)

world <- ne_countries(scale = "medium", returnclass = "sf")
world$id <- world$iso_a3

as.character(unique(dat$country[!(dat$id %in% world$iso_a3)]))

require(dplyr)

for (tt in round(seq(min(dat$date), max(dat$date), length.out = 10))) {
  print(tt)
  jpeg(paste0("plots/", tt, ".jpg"), res=300, width = 20, height = 20, units = "cm")
  print(ggplot(data = left_join(world, dat[dat$date==tt,c("id","school_closing")], by="id")) +
    geom_sf(aes(fill = school_closing)) + scale_color_ordinal() + ggtitle(tt))
  dev.off()
}


gg <- ggplot(data=dat, aes(y=id, x=date))
for (p in policies) print(gg + geom_tile(aes_string(fill=p)) + ggtitle(p)) + geom_text(aes(x=as.Date("2020-03-01"), y="ITA", label="ITA"))
gg + geom_tile(aes_string(fill="stringency_index")) + ggtitle("stringency") + scale_fill_gradient(low="green", high="red")

hclust()

sort(table(dat$id, useNA = "ifany"))
once <- distinct(dat[,c("id", "country", "lat", "lng", colnames(dat)[grepl("^pop", colnames(dat))])]) %>% as.data.frame()
once[is.na(once$lat),]
dat[dat$id=="USA",colnames(dat)[grepl("^pop", colnames(dat))]] %>%
require(countrycode)
require(ISOcodes)
table(dat$state %in% ISO_3166_2$Name)

table( %in% trimws(ISOcodes::ISO_3166_2$Name))
setdiff(dat$country, trimws(ISOcodes::ISO_3166_2$Name))
countrycode(dat$state, origin = "region", destination = "iso3")

require(StandardizeText)
standardize.countrynames(dat$state, standard = "iso", suggest = "auto")
data(country.regex)
country.regex


dat$stringency_index
ggplot(dat[,c("country","lng","lat")] %>% distinct()) + geom_point(aes(x=lng, y=lat, color=country)) + coord_fixed()

summary(dat)

ggplot(dat) + geom_line(aes(x=date, y=log(dat$tests+1), group=id))

ggplot(dat) + geom_smooth(aes(x=date, y=log(1+confirmed), group=id, color=country), method = "loess", se = F)
dat$lat
sort(table(dat$state, useNA = "ifany"))

dat$country[is.na(dat$state)]
### https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports_us
### csv giornalieri, solo per gli stati federali degli USA, da inizio aprile a oggi
### include ospedalizzati

### https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
### csv separati per:
### casi negli USA
### casi nel resto del mondo
### morti negli USA
### morti nel resto del mondo
### 

urls <- read_html("https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series") %>% html_nodes("td.content a") %>% html_attr("href")
urls <- urls[grepl("csv$", urls)]
urls <- sub("^.*?CSSEGISandData/COVID-19", "https://github.com/CSSEGISandData/COVID-19", urls)
csse_covid_19_time_series <- lapply(urls, read.csv)

str(csse_covid_19_time_series[[1]])

## https://github.com/CSSEGISandData/COVID-19/tree/master/who_covid_19_situation_reports

### https://github.com/CSSEGISandData/COVID-19/tree/master/who_covid_19_situation_reports/who_covid_19_sit_rep_pdfs
### solo pdfs

### https://github.com/CSSEGISandData/COVID-19/tree/master/who_covid_19_situation_reports/who_covid_19_sit_rep_time_series
### csv, tutti gli stati, dal 1/21/2020 al 4/3/2020 (fine gennaio-inizio aprile)
