
# require(rvest)
# require(data.table)
# require(pbapply)
# require(parallel)
# cl <- detectCores()
# 
# # https://github.com/CSSEGISandData/COVID-19
# 
# ## https://github.com/CSSEGISandData/COVID-19/tree/master/archived_data
# # deprecated
# 
# ## https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
# 
# ### https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports
# ### csv giornalieri, con casi|morti|guariti in colonna delle varie nazioni in riga
# ### esclude gli USA
# 
# fmtin <- "(\\d+)-(\\d+)-(\\d+).csv"
# repo <- "CSSEGISandData/COVID-19"
# urls <- paste0("https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports", c("", "_us"))
# files <- lapply(urls, function(u) read_html(u) %>% html_nodes("td.content a") %>% html_attr("href")) %>% unlist()
# files <- sub("/blob/", "/raw/", sub("^/", "https://github.com/", files[grepl(fmtin, files)]))
# dailyrep <- pblapply(files, function(v) {
#   aux <- read.csv(v)
#   colnames(aux) <- sub("^Lon.*$", "Lon", sub("^Lat.*$", "Lat", gsub("\\.", "_", colnames(aux))))
#   aux$Last_Update <- sub("^.*?(\\d+)-(\\d+)-(\\d+)\\.csv$", "\\1-\\2-\\3", v) %>% as.Date(tryFormats = c("%m-%d-%Y"))
#   aux
# }, cl=cl) %>% rbindlist(fill = T) %>% as.data.frame()
# summary(dailyrep)

# packs <- c("coronavirus", "covid19.analytics", "covid19italy", "COVID19", "covid19us", "covid19france")

dat <- as.data.frame(COVID19::covid19(level=1))

require(ggplot2)

cumul <- c("deaths", "confirmed", "tests", "recovered")
instant <- c("hosp", "icu", "vent")
source("policies.R")
demo <- colnames(dat)[grepl("^pop", colnames(dat))]
geo <- c("country", "state", "city", "lat", "lng")
index <- "stringency_index"
id <- "id"
time <- "date"

for (p in policies) {
  dat[[p]] <- ordered(as.character(dat[[p]]), levels=names(policies.levels[[p]]))
}

# policies.pca <- prcomp(scale(dat[,policies]))
# matplot(apply(policies.pca$rotation^2,1,cumsum), type="l", ylim=c(0,1))
# ggplot(policies.pca$rotation[,1:2]) + geom_text(aes(x=PC1, y=PC2, label=rownames(policies.pca$rotation))) + coord_fixed()
# ggplot(cbind(as.data.frame(policies.pca$x), index=dat$stringency_index)) + geom_point(aes(x=PC1, y=PC2, color=index)) + scale_color_gradient(low = "green", high = "red")

require(reshape2)
stringency.wide <- do.call("cbind", lapply(policies, function(pol) {
  # pol <- policies[1]
  aux <- as.data.frame(dcast(dat, as.formula(paste(id, "~", time)), value.var=pol))
  colnames(aux) <- paste0(pol, colnames(aux))
  for (p in colnames(aux)[-1]) {
    aux[[p]] <- ordered(as.character(aux[[p]]), levels=names(policies.levels[[pol]]))
  }
  aux
}))
rownames(stringency.wide) <- stringency.wide[,1]
stringency.wide <- stringency.wide[,!grepl("id$", colnames(stringency.wide))]
colnames(stringency.wide)
require(cluster)
d <- cluster::daisy(stringency.wide, metric = "gower")
stringency.clusters <- lapply(c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"), function(m) hclust(d, method = m))
lapply(stringency.clusters, function(v) plot(v, hang=-1, main = v$method))
chosen <- which(unlist(sapply(stringency.clusters, function(v) v$method=="ward.D2")))

dat$id <- factor(dat$id, levels = as.character(rownames(stringency.wide)[stringency.clusters[[chosen]]$order]))

require(maps)
require("rnaturalearth")
require("rnaturalearthdata")

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
