getmaps <- function() {
  require(COVID19)
  require(rnaturalearth)
  require(countrycode)
  require(maps)
  require(dplyr)
  require(stringdist)
  require(sf)
  
  lngs <- c(-15, 28)
  lats <- c(36, 69.5)
  
  map.nazioni <- ne_countries(continent = "Europe", returnclass = "sf")
  map.nazioni$id.country <- map.nazioni$iso_a3
  map.nazioni$id.country[map.nazioni$name %>% grepl(pattern = "Kosovo")] <- "RKS"
  map.nazioni <- map.nazioni[!(map.nazioni$id.country %in% c("BLR", "ISL", "RUS", "UKR")),]
  map.nazioni <- map.nazioni[(map.nazioni$name != "Moldova"),]
  map.nazioni <- suppressWarnings(map.nazioni %>% st_crop(xmin=lngs[1], xmax=lngs[2], ymin=lats[1], ymax=lats[2]))
  
  dat <- covid19(ISO = map.nazioni$id.country, level = 1) %>% as.data.frame()
  dat$id.country <- dat$id %>% sub(pattern = ",.*", replacement = "")
  
  doreg <- c("BEL", "CHE", "GBR", "ITA", "SWE")
  # LVA non e' di interesse, le altre hanno dati regionali solo oltremare
  datreg <- covid19(ISO = doreg, level = 2) %>% as.data.frame()
  datreg$id.country <- datreg$id %>% sub(pattern = ",.*", replacement = "")
  
  # DNK, FRA e NLD hanno solo terre d'oltremare nei dati regionali
  # LVA e' poco di interesse
  # datreg$country <- datreg$id.country %>% countrycode(origin = "iso3c", destination = "country.name")
  # al momento quindi e' in ordine solo il nome della nazione e l'id della nazione
  doreg <- datreg$id.country %>% unique() %>% countrycode(origin = "iso3c", destination = "iso2c")
  map.regioni <- ne_states(iso_a2 = doreg, returnclass = "sf")
  map.regioni$id.country <- map.regioni$admin %>% countrycode(origin = "country.name", destination = "iso3c")
  
  # piccole correzioni per l'Italia che agevolano il matching
  aux <- (map.regioni$id.country=="ITA") & grepl("Trentino", map.regioni$region)
  map.regioni$region[aux] <- paste("P.A.", map.regioni$gns_name[aux])
  
  # e per il Belgio
  aux <- (map.regioni$id.country=="BEL")
  map.regioni$geonunit[aux] <- map.regioni$geonunit[aux] %>% gsub(pattern = "\\s*(Capital|Region)\\s*", replacement = "")
  
  # e il Regno Unito
  aux <- (map.regioni$id.country=="GBR")
  map.regioni$region[aux] <- map.regioni$region[aux] %>% sub(pattern = "^Greater\\s*", replacement = "")
  map.regioni$region[aux] <- map.regioni$region[aux] %>% sub(pattern = "^East$", replacement = "East of England")
  # table(map.regioni$region[map.regioni$id.country=="ITA"])
  
  geometry.bak <- map.regioni$geometry
  map.regioni$geometry <- NULL
  
  # prima quelli facili
  
  match <- matrix(
    c("BEL", "geonunit",
    "CHE", "name_tr",
    "GBR", "region",
    "ITA", "region",
    "SWE", "name"),
  byrow = T, ncol = 2)
  
  map.regioni$state <- ""
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
    
    repl <- reg.missin
    names(repl) <- reg.matche
    
    map.regioni$state[aux] <- map.regioni$state[aux] %>% recode(!!!repl)
  }
  
  map.regioni <- map.regioni %>% mutate(geometry = geometry.bak) %>% st_as_sf()
  
  map.regioni <- map.regioni %>% group_by(id.country, state) %>% summarise()
  
  for (cntry in unique(map.regioni$id.country)) {
    aux <- map.regioni$id.country==cntry
    tomerge <- map.regioni$state[aux] %>% unique() %>% setdiff(datreg$state[datreg$id.country==cntry])
    repl <- rep(paste("Rest of", cntry), length(tomerge))
    names(repl) <- tomerge
    if (length(tomerge) > 0) {
      map.regioni$state[aux] <- map.regioni$state[aux] %>% recode(!!!repl)
    }
  }
  
  map.regioni <- map.regioni %>% group_by(id.country, state) %>% summarise()
  map.regioni$regional <- T
  map.regioni$national <- F
  map.regioni$finest <- T
  
  # begin mod
  
  map.nazioni <- ne_states(iso_a2 = map.nazioni$iso_a2[map.nazioni$name!="Kosovo"], returnclass = "sf")
  map.nazioni <- ne_states(country = "Kosovo", returnclass = "sf") %>% rbind(map.nazioni)
  map.nazioni <- suppressWarnings(map.nazioni %>% st_crop(xmin=lngs[1], xmax=lngs[2], ymin=lats[1], ymax=lats[2]))
  map.nazioni$id.country <- map.nazioni$admin %>% countrycode(origin = "country.name", destination = "iso3c", warn = F)
  map.nazioni$id.country[map.nazioni$geonunit=="Kosovo"] <- "RKS"
  if (any(map.nazioni$id.country %>% is.na())) stop("missing countries")
  map.nazioni <- map.nazioni %>% group_by(id.country) %>% summarise()
  # end mod
  
  
  
  map.nazioni$state <- NA
  map.nazioni$regional <- F
  map.nazioni$national <- T
  map.nazioni$finest <- !(map.nazioni$id.country %in% map.regioni$id.country)
  
  map.nazioni <- map.nazioni[,colnames(map.regioni)]
  
  rbind(map.regioni, map.nazioni)
}
