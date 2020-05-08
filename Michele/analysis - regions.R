
require(COVID19)
require(dplyr)
require(ggplot2)
require(tidyr)
require(rlist)
load("Michele/lib/maps/regio.RData")
source("Michele/lib/lagdata.R")
source("Michele/lib/long2wide.R")

mapdata %>% filter(finest) %>% ggplot() + geom_sf(aes(fill=paste(national, regional, civic, is.bin)))

dat <- bind_rows(
  covid19(ISO = mapdata$id[mapdata$national], level = 1)
  ,covid19(ISO = mapdata$id[mapdata$regional] %>% sub(pattern = ",.*", replacement = "") %>% unique(), level = 2)
  # ,covid19(ISO = mapdata$id[mapdata$civic] %>% sub(pattern = ",.*", replacement = "") %>% unique(), level = 3)
) %>% as.data.frame()

mapdata %>%
  filter(civic) %>%
  left_join(dat %>% filter(date==Sys.Date()-1), by="id") %>%
  ggplot() + geom_sf(aes(fill=hosp/confirmed))

dat %>%
  filter(!is.na(city)) %>% summary()

dat$id.country <- dat$id %>% sub(pattern = ",.*", replacement = "")
dat$label <- dat[,c("id.country", "state")] %>% apply(1, function(v) v %>% na.omit() %>% paste(collapse = "-")) 
dat <- dat %>% arrange(country, state, city, date)

dat %>% filter((state=="Sardegna") & (country=="Italy") & (date==Sys.Date()-1))

# calcolo variazioni di S, I, R e index
aux <- c("confirmed", "recovered", "deaths", "stringency_index")
dat <- aux %>% lagdata(dataset = dat, save.lag = T, save.var = F)
# mi limito a imputare il dato iniziale a zero
dat <- dat %>% replace_na(aux %>% paste0(".lag1") %>% sapply(function(v) 0) %>% as.list())
# qui calcolo effettivamente le variazioni
dat <- aux %>% lagdata(dataset = dat, save.lag = F, save.var = T)

all(dat$stringency_index.var1 == dat %>% group_by(id.country, state) %>% mutate(stringency_index.lag1alt=lag(stringency_index, 1) %>% replace_na(0)) %>% mutate(stringency_index.var1alt=stringency_index-stringency_index.lag1alt) %>% ungroup() %>% select("stringency_index.var1alt") %>% as.data.frame() %>% as.matrix() %>% as.vector())

# smusso la serie storica degli index, i dati sono già ordinati
dat <- dat %>% group_by(id.country, state) %>%
  mutate(stringency_index_smooth=loess(stringency_index ~ as.numeric(date)) %>% predict()) %>% as.data.frame()

# calcolo i casi attivi
dat <- dat %>% group_by(id.country, state) %>%
  mutate(cases.active=cumsum(confirmed.var1-deaths.var1-recovered.var1)) %>%
  mutate(cases.active_smooth=loess(cases.active ~ as.numeric(date)) %>% predict()) %>% as.data.frame()



dat2 <- dat %>% filter(is.na(state)) %>% long2wide(id = "id.country", time = "date", vars = "stringency_index_smooth")
rownames(dat2) <- dat2$id.country
cl <- dat2[,colnames(dat2) %>% grepl(pattern = "\\d+-\\d+-\\d+")] %>% dist() %>% hclust(method = "ward.D2")
cl %>% plot(hang=-1, cex=0.5)
dat2 <- data.frame(id.country=dat2$id.country, meanindex=dat2[,colnames(dat2) %>% grepl(pattern = "\\d+-\\d+-\\d+")] %>% rowSums(na.rm = T))
for (p in 2:10) {
  dat2$cl <- cl %>% cutree(k=p)
  dat2$cl <- dat2 %>% group_by(cl=cl) %>% summarise(meanindex=mean(meanindex)) %>% mutate(cl2=order(meanindex)) %>% right_join(dat2, by="cl") %>% select("cl2") %>% as.matrix() %>% drop() %>% as.ordered()
  print(mapdata %>% as_tibble() %>% filter(national) %>%
          left_join(dat2[,c("id.country", "cl")], by="id.country") %>%
          st_as_sf() %>% ggplot() + geom_sf(aes(fill=cl)) + ggtitle(paste("# clusters =", p)))
  print(dat %>% as_tibble() %>% filter(is.na(state)) %>% left_join(dat2, by="id.country") %>%
    ggplot() + geom_path(aes(x=date, y=stringency_index_smooth, group=id.country, color=cl)) + ggtitle(paste("# clusters =", p)))
}

dat[dat$id.country=="ITA",] %>%
  group_by(date, state, regional=is.na(state)) %>%
  summarise(new=sum(confirmed.var1), cumul=sum(confirmed), index=mean(stringency_index)) %>%
  ggplot() +
  geom_rect(aes(ymin=0,ymax=1,xmin=date+13.5,xmax=date+14.5,fill=index)) +
  geom_smooth(aes(x=date, y=new/cumul, color=regional, group=state), se=F, size=0.5)