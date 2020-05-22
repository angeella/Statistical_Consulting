
require(plotly)
require(glmmTMB)
require(COVID19)
require(dplyr)
require(ggplot2)
require(rnaturalearth)
require(rnaturalearthdata)
require(tidyr)
require(sf)
require(Hmisc)
source("Michele/get.maps.R")
source("Michele/lib/phases.R")

dat <- bind_rows(
  covid19(
    country = c(
      "Portugal",
      "Spain",
      "France",
      "Switzerland",
      "Ireland",
      "Austria",
      "United Kingdom",
      "Belgium",
      "Netherlands",
      "Germany",
      "Denmark",
      "Sweden",
      "Norway",
      "Korea, South",
      "Singapore"
    ),
    level = 1),
  covid19(country = "Italy", level = 2)
  # ,covid19(ISO = mapdata$id[mapdata$civic] %>% sub(pattern = ",.*", replacement = "") %>% unique(), level = 3)
) %>%
  rename(
    country=administrative_area_level_1,
    region =administrative_area_level_2
  ) %>%
  arrange(country, region, date) %>%
  mutate(
    active=pmax(0, confirmed-deaths-recovered),
    confirmed.new=pmax(0, confirmed - lag(confirmed,1)),
    deaths.new=pmax(0, deaths - lag(deaths,1)),
    recovered.new=pmax(0, recovered - lag(recovered,1)),
    tests.new=pmax(0, tests - lag(tests,1))
  ) %>%
  mutate(
    active.future=Hmisc::Lag(active,-14),
    confirmed.new.future=Hmisc::Lag(confirmed.new,-14),
    deaths.new.future=Hmisc::Lag(deaths.new,-14),
    recovered.new.future=Hmisc::Lag(recovered.new,-14),
    tests.new.future=Hmisc::Lag(tests.new,-14),
    hosp.future=Hmisc::Lag(hosp,-14),
    vent.future=Hmisc::Lag(vent,-14),
    icu.future=Hmisc::Lag(icu,-14),
    tests.future=Hmisc::Lag(tests,-14)
  ) %>%
  as.data.frame() %>%
  calc.phases() %>%
  mutate(
    region = region %>% replace_na("country") %>% factor(levels = c("country", unique(region) %>% na.omit())),
    date = date %>% as.Date()
  ) %>%
  filter(!is.na(active.future))

summary(dat)

map <- get.maps(dat)

dat <- dat %>%
  left_join(
    map %>%
      as.data.frame() %>%
      mutate(areakm2=(geometry %>% st_area() %>% as.numeric())*1e-6) %>%
      select(country, region, areakm2),
    by=c("country", "region")
  ) %>%
  mutate(denspop=population/areakm2)

colnames(dat)

model <- glmmTMB(
  active.future ~
    offset(log(0.5 + active)) +
    # school_closing +
    # workplace_closing +
    # cancel_events +
    # gatherings_restrictions +
    # transport_closing +
    # stay_home_restrictions +
    # internal_movement_restrictions +
    # international_movement_restrictions +
    # information_campaigns +
    # testing_policy +
    # contact_tracing +
    phase +
    # country +
    (phase-1|region) +
    log(population) +
    log(tests.new + 0.5) +
    log(tests.new.future + 0.5) +
    log(tests.future - tests + 0.5) +
    log(denspop),
  data = dat %>% filter(country=="Italy"),
  family = poisson()
)

check_overdispersion(model)

summary(model)

est <- ranef(model)[[1]]$`region:country` %>%
  mutate(
    country=rownames(.) %>% sub(pattern = ".*:", replacement = ""),
    region=rownames(.) %>% sub(pattern = ":.*", replacement = "")
  ) %>%
  rename(
    phase.region=phase
  ) %>%
  left_join(
    ranef(model)[[1]]$country %>%
      mutate(
        country=rownames(.)
      ) %>%
      rename(phase.country=phase),
    by="country"
  ) %>%
  mutate(phase = phase.region + phase.country + (
    effect("phase", model) %>%
      as.data.frame() %>%
      summarise(phase=log(fit[phase==1]/fit[phase==0])) %>%
      unlist()
  )) %>%
  select(country, region, phase)

map %>%
  left_join(
    est,
    by=c("country", "region")
  ) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill=(exp(phase)-1)*100), lwd=0) +
  xlim(-15, 25) +
  ylim(30, 70) +
  scale_fill_gradientn(colors=c("darkgreen", "green", "yellow", "orange", "red"))

summary(model)
