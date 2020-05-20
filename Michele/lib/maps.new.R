getmaps <- function() {
  require(COVID19)
  require(dplyr)
  require(ggplot2)
  require(countrycode)
  require(rnaturalearth)
  require(rnaturalearthdata)
  require(sf)
  
  dat <- 1:2 %>%
    lapply(covid19, country=NULL) %>%
    bind_rows()
  dat %>%
    filter(administrative_area_level_2=="Greenland") %>%
    as.data.frame()
  dat %>%
      
  maps <- ne_states(returnclass = "sf")
  maps %>%
    filter(admin=="Greenland")
  maps <- maps %>%
    mutate(admin=admin %>% recode("S. Sudan"="South Sudan")) %>%
    mutate(id1=admin %>% countrycode("country.name", "ecb"))
  geom.bak <- maps %>% st_geometry()
  geom.add <- st_sfc(st_point(c(142, 34)), st_point(c(-110, 20)), st_point(c(-82, 5)))
  st_crs(geom.add) <- st_crs(geom.bak)
  maps <- maps %>%
    st_set_geometry(NULL) %>%
    bind_rows(
      data.frame(id1=c("Diamond Princess", "Grand Princess", "MS Zaandam"))
    ) %>%
    st_set_geometry(c(geom.bak, geom.add))
  
  maps %>%
    filter(grepl("(Mayotte|G.*d.*l.*p|R.*uni|M.*r.*t.*n.*q)", name))
  
  maps %>%
    mutate(id2 = region %>% countrycode("iso3c", "ecb")) %>%
    filter(!is.na(id2) & !is.na(id1) & (id2!=id1))
  
  # maps %>%
  #   ggplot() +
  #   geom_sf(aes(fill=id1), lwd=0) +
  #   theme(legend.position = "none")
  
  mapnaz <- maps %>%
    group_by(id1) %>%
    summarise()
  
  # mapnaz %>%
  #   ggplot() +
  #   geom_sf(aes(fill=id1)) +
  #   theme(legend.position = "none")
  
  regs <- dat %>%
    filter(administrative_area_level==2) %>%
    as.data.frame() %>%
    select(id1, administrative_area_level_2, longitude, latitude) %>%
    distinct() %>%
    arrange(id1, administrative_area_level_2)
  
  # for (p in (dat$id1[dat$administrative_area_level==2] %>% unique())) {
  #   print(
  #     ggplot() +
  #       geom_sf(data=maps %>%
  #                 filter(id1 == p)) +
  #       geom_point(
  #         data=regs %>%
  #           filter(longitude<1833 & id1 == p),
  #         aes(x=longitude, y=latitude),
  #         color="red",
  #         shape=3
  #       ) +
  #       geom_point(
  #         data=maps %>%
  #           filter(id1 == p),
  #         aes(x=longitude, y=latitude),
  #         color="black",
  #         shape=4
  #       ) +
  #       ggtitle(p)
  #   )
  # }
  
  # aiuta ricodifiche per l'Italia
  aux <- grepl("(Trento|Bolzano)", maps$gns_name)
  maps$region[aux] <- maps$gns_name[aux]
  
  regs %>%
    select(id1) %>%
    distinct() %>%
    unlist() %>%
    as.vector()
  # "AI" "AS" "AU" "AW" "BE" "BL" "BM" "BQ" "CA" "CH" "CN" "CW" "CZ" "FK" "FO" "FR"
  # "GB" "GE" "GI" "GL" "GP" "GU" "HK" "IM" "IN" "IT" "JE" "KY" "LV" "MN" "MO" "MP"
  # "MQ" "MS" "MX" "PR" "RE" "RU" "SE" "SX" "TC" "US" "VG" "WF" "YT" "ZA"
  
  cands <- list(
    AU=c("name", "gn_name", "gns_name", "name_de", "name_en"),
    BE=c("name", "woe_name", "geonunit", "name_en"),
    CA=c("name", "woe_label", "woe_name", "gn_name", "gns_name", "name_en"),
    CH=c("name", "woe_name", "gn_name", "gns_name", "name_en"),
    CN=c("woe_label", "gn_name", "name_en"),
    CZ=c("name", "woe_label", "woe_name", "gn_name", "gns_name"),
    DK=c("admin", "geonunit"),
    FR=c("region"),
    GB=c("name_alt", "region", "woe_name", "gn_name", "gns_name", "region_sub", "name_de", "name_en"),
    IN=c("name", "woe_label", "woe_name", "gn_name", "gns_name", "name_en"),
    IT=c("region"),
    LV=c("gn_name", "gns_name")
  )
  
  
  
  needed <- regs %>%
    filter(id1=="HK")
  
  c(
    "Aruba",
    "Bonaire, Sint Eustatius and Saba",
    
  ) %>% countrycode("country.name", "ecb")
  
  avail <- maps %>%
    filter(id1=="HK")
  
  sapply(
    avail %>%
      as.data.frame(),
    function(col) {
      all(sapply(c("Aruba", "A"), function(v) {
        any(grepl(v, col))
      }))
    }) %>%
    which() %>%
    names()
  
  avail <- avail %>%
    group_by(geonunit) %>%
    summarise()
  avail %>%
    ggplot() +
    geom_sf()
  table(amatch(needed$administrative_area_level_2, avail$geonunit %>% na.omit() %>% unique(), method = "lcs", maxDist = Inf))
}
