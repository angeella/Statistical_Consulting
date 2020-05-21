
require(COVID19)
require(dplyr)
require(tidyr)
require(ramps)
require(ggplot2)
require(maps)
require(rnaturalearth)
require(rnaturalearthdata)
require(sf)
require(countrycode)

dat <- 1:2 %>%
  lapply(covid19, country=c("Netherlands", "Ireland", "Norway", "Portugal", "Belgium", "Denmark", "Spain", "Switzerland", "Austria", "France", "Italy", "Sweden", "Germany", "Korea, South", "Singapore", "United Kingdom")) %>%
  bind_rows() %>%
  rename(
    country=administrative_area_level_1,
    region =administrative_area_level_2
    ) %>%
  mutate(
    ecb = country %>% countrycode("country.name", "ecb"),
    region = ifelse(administrative_area_level==1,"country",region),
    date = date %>% as.Date(),
    active = pmax(confirmed - deaths - recovered, 0)
  ) %>%
  arrange(country, region, date)

map <- ne_countries(returnclass = "sf") %>%
  filter(sovereignt != "Somaliland") %>%
  mutate(ecb = sovereignt %>% countrycode("country.name", "ecb")) %>%
  filter(ecb %in% dat$ecb) %>%
  st_crop(xmin=-17, xmax=10000, ymin=-20, ymax=68.62)
# tutto questo per Singapore
map <- map %>%
  as.data.frame() %>%
  bind_rows(data.frame(ecb="Singapore" %>% countrycode("country.name", "ecb")) %>% st_set_geometry(st_point(c(103.5640535, 1.3139843)) %>% st_sfc())) %>%
  st_as_sf() %>%
  st_set_crs(st_crs(map))

dat <- dat %>%
  left_join(
    map %>%
      cbind(st_centroid(.) %>%
      st_coordinates()) %>%
      rename(longitude.alt=X, latitude.alt=Y) %>%
      select(ecb, latitude.alt, longitude.alt) %>%
      st_set_geometry(NULL) %>%
      as.data.frame() %>%
      mutate(administrative_area_level=1),
    by=c("ecb", "administrative_area_level")
  ) %>%
  mutate(
    latitude = ifelse(administrative_area_level==1, latitude.alt, latitude),
    longitude = ifelse(administrative_area_level==1, longitude.alt, longitude)
  ) %>%
  select(-latitude.alt, -longitude.alt)

dat <- dat %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  filter((id %in% c("SGP", "KOR")) | (((latitude > 37) & (longitude > -20) & ((latitude < 60) | (longitude > 0)))))

map %>%
  ggplot() +
  geom_sf() +
  geom_point(data=dat, aes(x=longitude, y=latitude, color=administrative_area_level)) +
  xlim(-15, 20) +
  ylim(35, 65)

dat <- dat %>%
  left_join(
    dat %>%
      filter(confirmed >= 10) %>%
      group_by(country, region) %>%
      summarise(datestart = min(date)),
    by=c("country", "region")
  ) %>%
  mutate(
    date2 = date - datestart
  )

dat <- dat %>%
  filter(administrative_area_level==1 | country=="Italy")

dat <- dat %>%
  group_by(id) %>%
  mutate(
    active.diff = active - lag(active),
    confis.diff = confirmed - lag(confirmed),
    deaths.diff = deaths - lag(deaths),
    recovs.diff = recovered - lag(recovered),
    tests.diff = tests - lag(tests)
  )

bulkdata <- dat %>%
  filter(confirmed>0 & !is.na(confis.diff)) %>%
  group_by(country, region) %>%
  summarise(
    zeroes = mean(confis.diff <= 0, na.rm=T),
    bulk.avg = mean(ifelse(confis.diff <= 0, NA, confis.diff), na.rm=T),
    bulk.var =  var(ifelse(confis.diff <= 0, NA, confis.diff), na.rm=T),
    all.avg =  mean(ifelse(confis.diff >= 0, confis.diff, NA), na.rm=T),
    all.var =   var(ifelse(confis.diff >= 0, confis.diff, NA), na.rm=T),
    pop=mean(population),
    cas=mean(ifelse(active>=0,active,NA),na.rm=T),
    n=n()
  ) %>%
  as.data.frame()

bulkdata %>%
  ggplot() +
  geom_point(aes(y=zeroes, x=log10(pop), color=country, size=region=="country"))

require(ggplot2)

# da tenere
dat %>%
  filter(administrative_area_level==2 & country=="Italy") %>%
  ggplot() +
  geom_line(aes(x=date, y=log10(active), group=region)) +
  scale_x_date(date_labels = "%b %d") +
  geom_vline(xintercept = as.Date(c("2020-03-10", "2020-05-04")) + 10)

require(lme4)
require(boot)

dat <- dat %>%
  filter(administrative_area_level==2 & country=="Italy") %>%
  arrange(id, date) %>%
  group_by(id) %>%
  mutate(
    active.lag1=lag(active,1),
    active.lag10=lag(active,10),
    active.lag14=lag(active,14)
  ) %>%
  mutate(
    myoffset=boot::logit((active.lag10+0.5)/(population+1)),
    phase=factor((date-10 >= "2020-03-10") + (date-10 >= "2020-05-04"))
  )

model1 <- glm(
  cbind(active, population - active) ~
    offset(myoffset) +
    region +
    # log(population) +
    log(tests + 1) +
    lag(log(tests + 1),10) +
    phase,
  family = binomial(link = "logit"),
  data=dat)

require(effects)

data.frame(effect("phase", model1))

data.frame(effect("region", model1)) %>%
  left_join(dat %>% filter(date=="2020-04-15"), by="region") %>%
  ggplot() +
  geom_point(aes(x=longitude, y=latitude, color=fit), size=10) 

summary(model1)

# model <- glmmTMB(
#   log(confirmed + 1) - lag(log(confirmed + 1),14) ~ log(population) + ar1(date2|id),
#   data = dat,
#   family = gaussian()
# )
# 
# summary(model)
        