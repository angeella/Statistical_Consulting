
source("Michele/lib/maps.R")
source("Michele/lib/lagdata.R")
source("Michele/lib/long2wide.R")
require(ggplot2)

mapdata <- getmaps()
mapdata$is.bin <- grepl("^Rest of", mapdata$state)
mapdata[mapdata$national,] %>% ggplot() + geom_sf(aes(fill=id.country)) + theme(legend.position = "none")
mapdata[mapdata$regional,] %>% ggplot() + geom_sf(aes(fill=is.bin))
mapdata[mapdata$finest,] %>% ggplot() + geom_sf(aes(fill=regional, lty = regional))

dat <- bind_rows(
  covid19(ISO = mapdata$id.country[mapdata$national], level = 1),
  covid19(ISO = mapdata$id.country[mapdata$regional], level = 2)
) %>% as.data.frame()

dat$id.country <- dat$id %>% sub(pattern = ",.*", replacement = "")

dat <- c("confirmed", "recovered", "deaths", "stringency_index") %>%
  lagdata(dataset = dat, save.var = T, save.lag = F)

dat <- dat %>% group_by(id.country, state) %>% arrange(id.country, state, date) %>%
  mutate(stringency_index_smooth=loess(stringency_index ~ as.numeric(date)) %>% predict())

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