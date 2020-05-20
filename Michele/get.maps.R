get.maps <- function(dataset) {
  ne_countries(returnclass = "sf") %>%
    mutate(
      id1 = admin %>% countrycode("country.name", "ecb"),
      administrative_area_level_2 = as.character(NA),
      administrative_area_level = 1
    ) %>%
    filter(id1 %in% dataset$id1) %>%
    as.data.frame() %>%
    bind_rows(
      ne_states(country = "Italy", returnclass = "sf") %>%
        mutate(region = ifelse(grepl("Trentino", region), paste("P.A.", gns_name), region)) %>%
        group_by(region) %>%
        summarise() %>%
        as.data.frame() %>%
        mutate(
          id1 = "Italy" %>% countrycode("country.name", "ecb"),
          administrative_area_level = 2,
          administrative_area_level_2 = {
            aux <- dataset %>%
              filter(administrative_area_level==2 & administrative_area_level_1=="Italy") %>%
              select(administrative_area_level_2) %>%
              distinct() %>% unlist()
            aux[region %>%
                  amatch(aux, method = "lv", maxDist=Inf)]
          }
        )
    ) %>%
    as.data.frame() %>%
    st_as_sf()
}