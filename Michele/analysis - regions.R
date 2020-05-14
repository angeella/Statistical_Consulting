
require(COVID19)
require(dplyr)
require(ggplot2)
require(tidyr)
require(sf)
load("Michele/lib/maps/regio.RData")
source("Michele/lib/lagdata.R")
source("Michele/lib/long2wide.R")
source("Michele/lib/oxford.R")
source("Michele/lib/policies.R")
source("Michele/lib/csse.R")
source("Michele/lib/mobility.R")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")[-1]

mapdata <- mapdata %>%
  mutate(tipo=ifelse(is.bin, "raccoglitori", ifelse(civic, "province", ifelse(regional, "regioni", "stati"))))

png("Michele/plots/mappaall.png", width = 20, height = 15, units = "cm", res=200)
mapdata %>%
  filter(finest) %>%
  ggplot() +
  geom_sf(aes(fill=tipo), lwd=0.5, color="black") +
  scale_fill_manual(values=cbPalette)
dev.off()

dat <- bind_rows(
  covid19(ISO = mapdata$id[mapdata$national], level = 1)
  ,covid19(ISO = mapdata$id[mapdata$regional] %>% sub(pattern = ",.*", replacement = "") %>% unique(), level = 2)
  # ,covid19(ISO = mapdata$id[mapdata$civic] %>% sub(pattern = ",.*", replacement = "") %>% unique(), level = 3)
) %>% as.data.frame() %>%
  mutate(national=is.na(state), regional=!is.na(state)) %>%
  arrange(id, date) %>%
  oxford() %>%
  csse() %>%
  mobility()

dat <- dat %>%
  mutate(
    active=confirmed - deaths - recovered,
    ox.active=ox.confirmed - ox.deaths - ox.recovered
  )

dat %>%
  filter(abs(active-ox.active) > 0.1*pmax(abs(active), abs(ox.active))) %>%
  ggplot() +
  geom_point(aes(
    x=active,
    y=ox.active,
    color=country
  )) +
  geom_abline(intercept = 0, slope = 1)

for (p in policies) {
  dat[[p]] <- dat[[p]] %>% ordered(levels=policies.levels[[p]] %>% names())
}

dat$stringency_index.res <-
  dat[,c("stringency_index", policies)] %>%
  glm(formula = stringency_index/100 ~ ., family = quasibinomial(link = "probit")) %>%
  fitted()
dat <- dat %>% mutate(stringency_index.res=stringency_index - 100*stringency_index.res)

dat %>% filter(national) %>%
  ggplot() +
  geom_boxplot(aes(x=stringency_index.res, y=id))

dat %>% filter(national) %>% ggplot() + geom_tile(aes(x=date, y=id, fill=C1_School.closing %>% ordered())) + ggtitle("Oxford data")# + theme(legend.position = "none")

png("Michele/plots/scuole.png", width = 20, height = 15, units = "cm", res=200)
dat %>%
  filter(national) %>%
  ggplot() +
  geom_tile(aes(x=date, y=country, fill=school_closing)) +
  # ggtitle("COVID19 R Package") + # + theme(legend.position = "none")
  scale_fill_manual(values=c("#2dc937", "#e7b416", "#db7b2b", "#cc3232")) +
  scale_x_date(date_labels = "%b %d") +
  xlab("") + ylab("") +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.ticks.y.left = element_blank(), axis.text = element_text(size=10)) +
  ggtitle("")
dev.off()

# dat %>% filter(national) %>% left_join(mapdata, by="id") %>%
#   st_as_sf() %>%
#   ggplot(cex=0.5) +
#   facet_wrap(~ date) +
#   geom_sf(aes(fill=stringency_index), lwd=0)

dat <- dat %>% mutate(cases.active=confirmed-deaths-recovered)

dat %>% filter(cases.active<0) %>% select(id, cases.active) %>% distinct()

dat <- dat %>% left_join(
  dat %>% filter(regional) %>% group_by(country, date) %>%
    summarise(
      cases.active.reg=sum(cases.active, na.rm = T),
      state=as.character(NA), city=as.character(NA)
    ),
  by=c("country", "state", "city", "date")
)

sapply(policies, function(pol) {
  print(
    dat %>% filter(regional & (country=="Italy")) %>%
      ggplot() +
      geom_tile(aes_string(x="date", y="id", fill=pol)) +
      ggtitle(pol)
  )
})

dat %>% filter(!is.na(cases.active.reg)) %>% ggplot() +
  geom_path(aes(group=id, x=cases.active, y=cases.active.reg, color=country))

sma <- function(y, k) {
  if (k <= 1) {
    (y + lag(y, 1))/2
  } else {
    sma(sma(y, k - 1), 1)
  }
}

sapply(policies, function(pol) {
  print(dat %>% filter(T) %>% ggplot() +
          geom_tile(aes_string(x="date", y="id", fill=pol)) +
          ggtitle(pol))
})

cl <- dat %>% filter(national) %>% long2wide(id="id", time="date", vars="stringency_index")
rownames(cl) <- cl$id
cl$id <- NULL
cl <- cl %>% dist() %>% hclust(method = "complete")
cl <- list(order=cl$labels[cl$order])
cl$todo <- dat$id %>% unique()

dat$id <- dat$id %>% factor(levels=sapply(cl$order, function(v) {
  cl$todo[grepl(v, cl$todo)]
}) %>% unlist() %>% as.vector())

sapply(policies, function(pol) {
  print(dat %>% filter(T) %>% ggplot() +
          geom_tile(aes_string(x="date", y="id", fill=pol)) +
          ggtitle(pol))
})

dat %>% ggplot() + geom_path(aes(y=id, x=date, color=regional))

dat %>% filter(T) %>% ggplot() +
  geom_tile(aes(x=date, y=id, fill=tests>0))

dat %>% filter(national & ((tests > 0) | (confirmed > 0))) %>%
  group_by(id) %>% mutate(
    conf=sma(log(1+confirmed), 5),
    reco=sma(log(1+recovered), 5),
    deaf=sma(log(1+deaths), 5)
  ) %>%
  ggplot() +
  geom_path(aes(x=date, y=conf, group=id), color="blue") +
  geom_path(aes(x=date, y=reco, group=id), color="green") +
  geom_path(aes(x=date, y=deaf, group=id), color="red")

dat %>% filter(national) %>% ggplot() + geom_path(aes(x=date, y=log(1+mkt_close), group=id))
dat %>% filter(national) %>% ggplot() + geom_path(aes(x=date, y=log(1+mkt_volume), group=id))

dat %>% filter(national) %>% ggplot() +
  geom_tile(aes(x=date, y=id, fill=stringency_index)) +
  scale_fill_gradient(low="green", high="salmon")

dat %>% filter(regional)

# calcolo variazioni di S, I, R e index
aux <- c("confirmed", "recovered", "deaths", "stringency_index")
dat <- aux %>% lagdata(dataset = dat, save.lag = T, save.var = F)
# mi limito a imputare il dato iniziale a zero
dat <- dat %>% replace_na(aux %>% paste0(".lag1") %>% sapply(function(v) 0) %>% as.list())
# qui calcolo effettivamente le variazioni
dat <- aux %>% lagdata(dataset = dat, save.lag = F, save.var = T)

all(dat$stringency_index.var1 == dat %>% group_by(id.country, state) %>% mutate(stringency_index.lag1alt=lag(stringency_index, 1) %>% replace_na(0)) %>% mutate(stringency_index.var1alt=stringency_index-stringency_index.lag1alt) %>% ungroup() %>% select("stringency_index.var1alt") %>% as.data.frame() %>% as.matrix() %>% as.vector())

# smusso la serie storica degli index, i dati sono gi� ordinati
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