
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
require(lme4)
source("Michele/get.maps.R")
source("Michele/lib/phases.R")
source("Angela/divide_var.R")

wb <- c(
  "pop_0_14"="SP.POP.0014.TO.ZS",
  "pop_15_64"="SP.POP.1564.TO.ZS",
  "pop_65_up"="SP.POP.65UP.TO.ZS",
  "pop_density"="EN.POP.DNST",
  "pop_death_rate"="SP.DYN.CDRT.IN",
  "gdp"="NY.GDP.PCAP.CD",
  "health_exp"="SP.DYN.LE00.IN",
  "hosp_beds"="SH.MED.BEDS.ZS"
)

dat <- bind_rows(
  covid19(
    country = c(
      "Portugal", "Spain", "France", "Switzerland", "Ireland", "Austria", "United Kingdom", "Belgium", "Netherlands", "Germany", "Denmark", "Sweden", "Norway", "Korea, South", "Singapore"
    ), level = 1, wb=wb),
  covid19(country = "Italy", level = 2, wb=wb)
) %>%
  rename(
    country=administrative_area_level_1,
    region=administrative_area_level_2
  ) %>%
  arrange(country, region, date) %>%
  mutate(
    active=pmax(0, confirmed-deaths-recovered),
    date = date %>% as.Date(),
    region = region %>% replace_na("country")
  ) %>%
  group_by(country, region) %>%
  mutate(
    active.future=Hmisc::Lag(active,-14),
    tests.future=Hmisc::Lag(tests,-14),
    tests.past=Hmisc::Lag(tests,14)
  ) %>%
  as.data.frame() %>%
  calc.phases() %>%
  filter(confirmed > 30)

map <- get.maps(dat)

# save(map, file="Michele/maps.RData")

dat <- dat %>%
  filter(country=="Italy" & region!="country")

dat %>%
  ggplot() +
  geom_line(aes(x=date, y=tests, group=id, color=region))

dat <- dat %>%
  mutate(
    denominator=log(0.5 + active),
    log_past_tests=log(tests - tests.past + 0.5),
    log_future_tests=log(tests.future - tests + 0.5),
    log_pop=log(population),
    log_dens=log(pop_density),
    overdispersion=1:nrow(dat)
  )

f.def <- active.future ~
  offset(denominator) +
  phase +
  log_past_tests +
  # log_future_tests +
  (phase+1|region) +
  (1|date) +
  (1|overdispersion) # +
  # log_pop #+
  # log_dens

model <- glmer(
  f.def,
  data = dat,
  family = poisson()
)

require(ciTools)

dat <- dat %>% right_join(model@frame %>% dplyr::select(region, date), by=c("region", "date"))
dat <- ciTools::add_pi(dat, model)
dat <- dat %>%
  rename(
    predlower=`LPB0.025`,
    predpoint=pred,
    predupper=`UPB0.975`
    )
dat <- dat %>%
  mutate(denominator=exp(denominator)) %>%
  mutate(
    ratelower=predlower/denominator,
    ratepoint=predpoint/denominator,
    rateupper=predupper/denominator
  )

save(dat, model, file="Michele/finaleregioni.RData")
