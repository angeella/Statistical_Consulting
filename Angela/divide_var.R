#Divide variables between
#1. POLICY LOCKDOWN
#2. FIX VARIABLES
#3. ECONOMIC
#4. HEALTH SYSTEM
#5. INDEX
#6. COVID

var_LD <- c("school_closing", "workplace_closing", "cancel_events", "gatherings_restrictions",
         "transport_closing", "stay_home_restrictions", "internal_movement_restrictions",
         "contact_tracing", "testing_policy")

var_FIX <- c("pop", "pop_65", "pop_density", "hosp_beds", "pop_death_rate",
             "gdp", "pop_urban", "surface_area")

var_EC <- c("E1_Income_support", "E2_Debt_contract_relief", "E3_Fiscal_measures",
            "E4_International_support")

var_HS <- c("H4_Emergency_investment_in_healthcare", "H5_Investment_in_vaccines")

index <- c("StringencyIndex","GovernmentResponseIndex", "EconomicSupportIndex", "R0mean" ,"ContainmentHealthIndex", "R0")

var_COVID <- c("confirmed", "deaths", "tests", "recovered", "hosp","vent", "icu")

save(var_LD,var_FIX,var_EC, var_HS,index, var_MOB,var_COVID,file = "Angela/Data/var.RData")

##Plus obviously country (id) and date

#Some notes:
#1. icu? number of hospitalized patients in icu 
#2. vent? Number of patients requiring invasive ventilation on date.
#3. hosp? Number of hospitalized patients on date.
#8. M1_Wildcard? Record policy announcements that do not fit anywhere else <- togli




