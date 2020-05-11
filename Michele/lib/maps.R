getmaps <- function(lngs=c(-15, 28), lats=c(36, 69.5), want.nations=T, want.regions=F, want.cities=F) {
  require(COVID19)
  require(ggplot2)
  require(rnaturalearth)
  require(countrycode)
  require(maps)
  require(dplyr)
  require(tidyr)
  require(stringdist)
  require(sf)
  
  `%notin%` <- Negate(`%in%`)
  
  map.nazioni <- ne_countries(continent = "Europe", returnclass = "sf") %>%
    mutate(id=iso_a3 %>% replace_na("RKS")) %>%
    filter(id %notin% c("BLR", "ISL", "RUS", "UKR")) %>%
    filter(name != "Moldova") %>%
    st_crop(xmin=lngs[1], xmax=lngs[2], ymin=lats[1], ymax=lats[2])
  
  datnaz <- covid19(ISO = map.nazioni$id, level = 1) %>%
    select(id, country) %>% distinct()
  
  map.nazioni <- map.nazioni %>%
    left_join(datnaz, by="id")
  
  ggplot() +
    geom_sf(data = map.nazioni) +
    geom_sf(data = map.nazioni %>% st_centroid())
  
  doreg <- datnaz$id %>%
    intersect(c("BEL", "CHE", "GBR", "ITA", "SWE"))
  
  # LVA non e' di interesse, le altre hanno dati regionali solo oltremare
  
  datreg <- covid19(ISO = doreg, level = 2) %>%
    select(id, country, state) %>% distinct() %>%
    mutate(id.country=id %>% sub(pattern = ",.*", replacement = ""))
  
  doreg <- datreg$id.country %>% sub(pattern = ",.*", replacement = "") %>% unique() %>%
    countrycode(origin = "iso3c", destination = "iso2c")
  map.regioni <- ne_states(iso_a2 = doreg, returnclass = "sf") %>%
    mutate(id.country=admin %>% countrycode(origin = "country.name", destination = "iso3c")) %>%
    left_join(datnaz, by=c("id.country"="id"))
  
  # piccole correzioni per l'Italia che agevolano il matching
  aux <- (map.regioni$id.country=="ITA") & grepl("Trentino", map.regioni$region)
  map.regioni$region[aux] <- paste("P.A.", map.regioni$gns_name[aux])
  
  # e il Regno Unito
  aux <- (map.regioni$id.country=="GBR")
  map.regioni$region[aux] <- map.regioni$region[aux] %>% sub(pattern = ".*London.*", replacement = "London")
  map.regioni$region[aux] <- map.regioni$region[aux] %>% sub(pattern = "^East$", replacement = "East of England")
  
  # backup dell'oggetto geometry
  
  geometry.bak <- map.regioni$geometry
  map.regioni <- map.regioni %>%
    st_set_geometry(NULL)
  
  match <- matrix(
    c("BEL", "geonunit",
    "CHE", "name_tr",
    "GBR", "region",
    "ITA", "region",
    "SWE", "name"),
  byrow = T, ncol = 2)
  
  map.regioni$state <- ""
  repl.all <- c()
  for (i in 1:nrow(match)) {
    cntry <- match[i,1]
    varmatch <- match[i,2]
    writeLines(paste0("processing ", cntry, ", matching..."))
    aux <- map.regioni$id.country==cntry
    map.regioni$state[aux] <- map.regioni[aux,varmatch]
    
    reg.needed <- datreg$state[datreg$id.country==cntry] %>% unique()
    reg.availa <- map.regioni$state[aux] %>% unique()
    reg.missin <- reg.needed %>% setdiff(reg.availa)
    reg.unwant <- reg.availa %>% setdiff(reg.needed)
    which.match <- stringdistmatrix(reg.missin, reg.unwant) %>% apply(1, which.min) %>% drop()
    reg.matche <- reg.unwant[which.match]
    
    writeLines(paste0("\t", reg.missin, " <-> ", reg.matche))
    
    prev <- names(repl.all)
    repl <- reg.missin
    repl.all <- c(repl.all, reg.missin)
    names(repl) <- reg.matche
    names(repl.all) <- c(prev, reg.matche)
    
    map.regioni$state[aux] <- map.regioni$state[aux] %>% recode(!!!repl)
  }
  
  map.regioni <- map.regioni %>%
    select(id.country, country, state) %>%
    st_set_geometry(geometry.bak) %>%
    left_join(datreg, by=c("id.country", "country", "state")) %>%
    mutate(id=ifelse(is.na(id), paste0(id.country, ", rest"), id)) %>%
    group_by(id, country) %>%
    summarise(state=paste(state %>% unique() %>% sort(), collapse = "+"))
  
  map.regioni$id.country <- NULL
  
  map.regioni %>%
    ggplot() +
    geom_sf(aes(fill=grepl("rest", id)))
  
  map.nazioni <- ne_states(iso_a2 = map.nazioni$iso_a2[map.nazioni$name!="Kosovo"], returnclass = "sf") %>%
    rbind(ne_states(country = "Kosovo", returnclass = "sf")) %>%
    st_crop(xmin=lngs[1], xmax=lngs[2], ymin=lats[1], ymax=lats[2]) %>%
    mutate(id=admin %>% countrycode(origin = "country.name", destination = "iso3c", warn=F)) %>%
    mutate(id=id %>% replace_na("RKS")) %>%
    group_by(id) %>% summarise() %>% left_join(datnaz, by="id")
  
  datcit <- covid19(ISO="ITA", level=3) %>%
    select(id, country, state, city) %>%
    distinct()
  
  map.citta <- ne_states(country = "Italy", returnclass = "sf") %>%
    mutate(id=iso_3166_2 %>% sub(pattern = "-", replacement = "A, ")) %>%
    left_join(datcit, by="id") %>%
    mutate(
      id=ifelse(is.na(city), "ITA, rest", id),
      country=country %>% replace_na("Italy"),
      state=state %>% replace_na("Sardegna"),
      city=ifelse(is.na(city), gn_name %>% sub(pattern = "Provincia di ", replacement = ""), city)
    ) %>% group_by(id, country, state) %>% summarise(city=city %>% unique() %>% paste(collapse = "+"))
  
  map.nazioni <- map.nazioni %>% mutate(
    state=as.character(NA),
    city=as.character(NA),
    civic=F,
    regional=F,
    national=T,
    finest=((country %notin% map.regioni$country) | !want.regions) & ((country %notin% map.citta$country) | !want.cities)
  ) %>% as_tibble()
  
  map.regioni <- map.regioni %>% mutate(
    city=as.character(NA),
    civic=F,
    regional=T,
    national=F,
    finest=(country %notin% map.citta$country) | !want.cities
  ) %>% as_tibble()
  
  map.citta <- map.citta %>% mutate(
    civic=T,
    regional=F,
    national=F,
    finest=T
  ) %>% as_tibble()
  
  bind_rows(map.nazioni, if (want.regions) map.regioni, if (want.cities) map.citta) %>%
    st_as_sf() %>%
    mutate(is.bin=grepl(", rest", id)) %>%
    select(id, national, regional, civic, finest, is.bin)
}

# mapdata <- getmaps(want.nations = T, want.regions = T, want.cities = T)
# save(mapdata, file="Michele/lib/maps/civic.RData")
# mapdata <- getmaps(want.nations = T, want.regions = T, want.cities = F)
# save(mapdata, file="Michele/lib/maps/regio.RData")
# mapdata <- getmaps(want.nations = T, want.regions = F, want.cities = F)
# save(mapdata, file="Michele/lib/maps/natio.RData")
