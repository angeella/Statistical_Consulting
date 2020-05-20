
require(COVID19)
require(dplyr)
require(countrycode)

require(sf)
require(maps)
require(ggplot2)
require(rnaturalearth)
require(rnaturalearthdata)

require(stringdist)

source("Michele/add.oxford.R")
source("Michele/add.mobility.R")
source("Michele/down.covid.R")
source("Michele/get.maps.R")

dat <- down.covid() %>%
  add.oxford() #%>%
  # add.mobility()

maps <- dat %>%
  get.maps()

require(plm)

dat <- dat %>%
  mutate(
    active=pmax(0, confirmed-deaths-recovered)
  )

clinic <- c("active", "confirmed", "recovered", "deaths", "hosp", "vent", "icu")
demo <- "population"
mobil <- c("retail_and_recreation_percent_change_from_baseline", "grocery_and_pharmacy_percent_change_from_baseline", "parks_percent_change_from_baseline", "transit_stations_percent_change_from_baseline", "workplaces_percent_change_from_baseline", "residential_percent_change_from_baseline")
econo <- c("E1_Income_support", "E2_Debt_contract_relief", "E3_Fiscal_measures", "H4_Emergency_investment_in_healthcare")
polic <- c("school_closing", "workplace_closing", "cancel_events", "gatherings_restrictions", "transport_closing", "stay_home_restrictions", "internal_movement_restrictions", "international_movement_restrictions", "information_campaigns", "testing_policy", "contact_tracing", "stringency_index")

f.plain <- paste(
  "lag(log(1+", clinic[1], "),-14) ~",
  paste(c(
    "log(population)",
    paste("offset(log(1+", clinic[1], "))"),
    polic,
    paste("log(1+", clinic[-1], ")")
    ,econo
  ), collapse = "+")
)

f.strumv <- paste(
  f.plain,
  "|",
  paste(
    paste("lag(",
          c(
            polic,
            paste("log(1+", clinic, ")")
            ,econo
          )
          ,",c(14,21))"),
    collapse = "+"
  )
) %>% as.formula()

rownames(dat) <- NULL # make.names(dat[,c("id", "date")] %>% apply(1, paste, collapse="___"), unique = TRUE)

res <- lm(
  formula = f.strumv,
  data = dat,
  effect = "twoways",
  model = "within"
)

dat %>%
  filter(date == "2020-03-20") %>%
  left_join(maps, by=c("id1", "administrative_area_level_2")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill=retail_and_recreation_percent_change_from_baseline))


