##Load Data
library("COVID19")
source("Angela/packages.R")
add.oxford <- function(dataset) {
  dataset$id1 <- dataset$iso_alpha_2
  oxford <- "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv" %>%
    read.csv() %>%
    rename(date=Date) %>%
    mutate(
      date = date %>%
        sub(pattern = "^(\\d{4})(\\d{2})(\\d{2})$", replacement = "\\1-\\2-\\3") %>%
        as.Date("%Y-%m-%d"),
      id1 = CountryName %>%
        countrycode("country.name", "ecb")
    )
  oxford <- oxford[,c("id1", "date", colnames(oxford)[grepl("^[EH]\\d", colnames(oxford)) & !grepl("[Ff]lag", colnames(oxford))])]
  oxford[is.na(oxford)] <- 0
  ordereds <- oxford[-c(1:2)] %>% sapply(function(v) if (is.numeric(v)) all(v < 10) else F) %>% which() %>% names()
  tologs <- colnames(oxford)[-c(1:2)] %>% setdiff(ordereds)
  for (p in ordereds) {
    oxford[[p]] <- oxford[[p]] %>% factor(ordered=F)
  }
  for (p in tologs) {
    oxford[[p]] <- log10(1+oxford[[p]])
  }
  
  # all(table(oxford$id1, oxford$date, useNA = "ifany") <= 1)
  
  colnames(oxford) <- colnames(oxford) %>% gsub(pattern = "\\.", replacement = "_")
  
  dataset %>%
    left_join(oxford, by=c("id1", "date"))
}

uno <- c("KOR", "SGP")
due <- c("DEU", "SWE")
tre <- c("CAN", "GRC", "PRT","USA")
quattro <- c("ESP", "GBR", "IRL", "ITA", "NLD")
cinque <- c("AUT", "BEL", "CHE", "DNK", "FIN", "FRA", "NOR")
states_to_sel <- c(uno,due,tre,quattro,cinque)
# Merge with World Bank data
wb <- c("gdp" = "NY.GDP.MKTP.CD", 
        "hosp_beds" = "SH.MED.BEDS.ZS",
        "pop" = "SP.POP.TOTL",
        "pop_density"= "EN.POP.DNST",
        "pop_65" = "SP.POP.65UP.TO.ZS",
        "surface_area" = "AG.SRF.TOTL.K2",
        "pop_urban" = "SP.URB.TOTL",
        "pop_death_rate" = "SP.DYN.CDRT.IN")
dat  <- covid19(states_to_sel,level = 1,wb = wb)

dat <- add.oxford(dat)

save(dat, file = "Angela/Data/db.RData")
