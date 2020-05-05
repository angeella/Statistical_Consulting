require(COVID19)
require(ggplot2)
require(dplyr)
require(countrycode)
require(ISOcodes)
require(stringr)
require(stringi)
require(stringdist)

# per mappe...
require(maps)
require(rnaturalearth)
require(rnaturalearthdata)

# librerie
source("Michele/lib/long2wide.R") 
source("Michele/lib/merger.R")
source("Michele/lib/policies.R") 
source("Michele/lib/lagdata.R") 
source("Angela/Compute_R0.R")
source("Angela/merge_dat_R0.R")
dat <- COVID19::covid19(level=1) %>% as.data.frame()
cumul <- c("deaths", "confirmed", "tests", "recovered")
instant <- c("hosp", "icu", "vent")
demo <- colnames(dat)[grepl("^pop", colnames(dat))]
geo <- c("country", "state", "city", "lat", "lng")
index <- "stringency_index"
id <- "id"
time <- "date"

dat <- dat %>% lagdata(vars=c(cumul, policies, index), lag = 1, save.lag = F, save.var = T)

dat <- merger(dat)

dat1 <- add_R0(dataset = dat)

#Divide variables between
#1. SOCIAL LOCKDOWN
#2. FIX VARIABLES
#3. ECONOMIC
#4. HEALTH SYSTEM
#5. INDEX
#6. MOBILITY
#7. COVID

colnames(dat1)

var_LD <- c("school_closing", "workplace_closing", "cancel_events", "gatherings_restrictions",
         "transport_closing", "stay_home_restrictions", "internal_movement_restrictions")
var_FIX <- c("state", "pop", "pop_65", "pop_age", "pop_density", "hosp_beds", "pop_death_rate",
             "gdp")

var_EC <- c("ox.E1_Income.support", "ox.E2_Debt.contract.relief", "ox.E3_Fiscal.measures",
            "ox.E4_International.support", "mkt_close", "mkt_volume")

var_HS <- c("information_campaigns", "testing_policy", "contact_tracing","ox.H4_Emergency.investment.in.healthcare",
           "ox.H5_Investment.in.vaccines", "health_exp", "health_exp_oop")

index <- c("ox.StringencyIndex","ox.LegacyStringencyIndex", "R0mean" ,"stringency_index")

var_MOB <- c("retail_and_recreation_percent_change_from_baseline", "grocery_and_pharmacy_percent_change_from_baseline",
             "parks_percent_change_from_baseline", "transit_stations_percent_change_from_baseline",
             "workplaces_percent_change_from_baseline", "residential_percent_change_from_baseline")

var_COVID <- c("confirmed", "deaths", "tests", "recovered", "wm.serious.critical", "hosp",
               "vent", "icu")


save(var_LD,var_FIX,var_EC, var_HS,index, var_MOB,var_COVID,file = "Angela/Data/var.RData")

##Plus obviously country (id) and date

#Question to analyze!!! <-
#1. icu? number of hospitalized patients in icu 
#2. vent? Number of patients requiring invasive ventilation on date.
#3. hosp? Number of hospitalized patients on date.
#4. mkt_close? Stock market price (Close).
#5. mkt_volume? Stock market volume.
#6. health_exp? Current health expenditure (% of GDP).
#7. health_exp_oop? Out-of-pocket expenditure (% of health expenditure).
#8. .var1?
#9. M1_Wildcard? Record policy announcements that do not fit anywhere else <- togli