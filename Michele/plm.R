require(COVID19)
require(dplyr)
require(plm)
require(ggplot2)
require(rnaturalearth)
require(sf)
require(maps)
require(countrycode)
require(nlme)

dat <- 1:2 %>%
  lapply(
    covid19,
    country=ne_countries(continent="Europe", returnclass = "sf") %>%
    as.data.frame() %>%
    select(admin) %>%
    distinct() %>%
    unlist() %>%
    as.vector(),
    verbose=F
  ) %>%
  bind_rows() %>%
  as.data.frame() %>%
  mutate(
    date=date %>% as.Date(),
    active=pmax(0, confirmed-deaths-recovered)
  ) %>%
  arrange(id, date) %>%
  rename(
    country=administrative_area_level_1,
    region=administrative_area_level_2
  )


response <- c("active", "recovered", "deaths", "confirmed", "tests", "hosp", "vent", "icu")
demo <- c("population")
policies <- c("school_closing",  "workplace_closing", "cancel_events", "gatherings_restrictions", "transport_closing", "stay_home_restrictions", "internal_movement_restrictions", "international_movement_restrictions", "information_campaigns", "testing_policy", "contact_tracing")
index <- c("stringency_index")

for (p in policies) {
  dat[[p]] <- dat[[p]] %>% factor(ordered = F)
}

lags.invar <- 7*(2:4)

f.plain <- paste(
  # "cbind(",
  paste("log(1+lag(", response[1], ",-14))-log(1+", response[1], ")", collapse = ","), # tra 14 giorni
  # ")",
  "~ 1 + stringency_index +",
  paste("log(1+", response[-1], ")", collapse = "+"),
  "+log(population) +",
  paste(policies, collapse = " + ")
)
f.invar <- paste(
  f.plain,
  "|",
  expand.grid(
    "lag(",
    c(
      "stringency_index",
      paste("log(1+", response, ")"),
      policies
    ), ", ", lags.invar, ")") %>%
    apply(1, paste, collapse="") %>%
    paste(collapse = "+")
)

# model0 <- plm(f, data = dat, model = "pooling", effect = "twoways")
model.ins <- plm(
  formula = as.formula(f.invar),
  data = dat %>%
    # filter(confirmed > 10) %>%
    pdata.frame(index = c("id", "date", "country")),
  model = "within",
  effect = "twoways"
)
model.ols <- plm(
  formula = as.formula(f.plain),
  data = dat %>% pdata.frame(index = c("id", "date", "country")),
  model = "within",
  effect = "twoways"
)
# model.rob <- pgmm(
#   formula = as.formula(f.invar),
#   data = dat %>% pdata.frame(index = c("id", "date", "country")),
#   effect = "twoways", model = "twosteps"
# )
# summary(model.rob)

usemod <- model.ins

summary(usemod)

res <- data.frame(fitted=fitted(usemod), resids=residuals(usemod), id=names(fitted(usemod)))
res$id <- res$id %>% as.character()
res$date <- rownames(res) %>% sub(pattern = "^.*?(\\d{4}-\\d{2}-\\d{2}).*?$", replacement = "\\1") %>% as.Date()
dat$fitted <- dat$resids <- NULL

dat <- dat %>%
  mutate(date=date %>% as.Date()) %>%
  left_join(res, by=c("id", "date"))


dat %>%
  filter(country=="Italy") %>%
  mutate(fitted=ifelse(date < "2020-03-10", NA, fitted)) %>%
  ggplot() +
  geom_line(aes(x=date, y=exp(fitted), group=id, color=region)) +
  scale_x_date(date_labels = "%b %d", limits = c("2020-03-")) #+
  # xlim(min="2020-03-01", ylim="2020-05-01")
summary(model)

require(maps)
require(rnaturalearth)
ne_countries(returnclass = "sf") %>%
  left_join(fixefs, by=c("iso_a3"="id")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill=fixef))

ests <- fitted(model)

ests <- data.frame(
  id=names(ests),
  est=ests %>% as.vector()
) %>% group_by(id) %>%
  mutate(date=min(dat$date) + 13 + 1:length(est)) %>%
  as.data.frame()

dat <- dat %>%
  left_join(ests, by=c("id", "date"))

dat$region <- dat$region %>% recode("0000"="nazionale")

dat %>%
  filter(country=="Italy") %>%
  ggplot() +
  geom_line(aes(x=date, y=est, group=region, color=region))
