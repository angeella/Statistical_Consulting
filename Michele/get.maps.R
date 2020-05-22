get.maps <- function(dataset) {
  require(countrycode)
  require(stringdist)
  map <- ne_states(iso_a2 = dataset$country %>% unique() %>% countrycode("country.name", "iso2c"), returnclass = "sf") %>%
    mutate(id1=admin %>% countrycode("country.name", "ecb")) %>%
    st_crop(xmin=-13, xmax=180, ymin=0, ymax=67) %>%
    mutate(
      region = ifelse(
        id1=="IT",
        ifelse(grepl("Trentino", region), paste("P.A.", gns_name), region),
        as.character(NA)
      )
    ) %>%
    group_by(id1, region) %>%
    summarise() %>%
    mutate(
      region = {
        aux <- dataset$region %>% levels() %>% setdiff("country")
        aux[region %>%
              amatch(aux, method = "lv", maxDist=Inf)]
      } %>% replace_na("country")
    ) %>%
    left_join(
      dataset %>%
        as.data.frame() %>%
        select(country) %>%
        distinct() %>%
        mutate(id1 = country %>% countrycode("country.name", "ecb")),
      by = "id1"
    ) %>%
    as.data.frame() %>%
    select(country, region, geometry) %>%
    st_as_sf()
  
  map %>%
    filter(country=="Italy") %>%
    ggplot() +
    geom_sf() +
    geom_sf_label(aes(label=region))
  
  map %>%
    group_by(country) %>%
    summarise() %>%
    ggplot() +
    geom_sf()
  
  map
}