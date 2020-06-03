rm(list=ls())
#Seth path 
path <- "~/GitHub/Statistical_Consulting/"

#load package
source(paste0(path,"Angela/packages.R"))
source(paste0(path, "Angela/compute_R0_ML.R"))
source(paste0(path,"Angela/utils.R"))
#load variables names
load("~/GitHub/Lockdown_policies_COVID19/Code/Angela/Data/var.RData")

#load data
load("~/GitHub/Lockdown_policies_COVID19/Code/Angela/Data/db.RData")

#Define Clusters 
Cl1 <- c("KOR", "SGP")
Cl2 <- c("DEU", "SWE")
Cl3 <- c("CAN", "GRC", "PRT", "USA")
Cl4 <- c("ESP", "GBR", "IRL", "ITA", "NLD")
Cl5 <- c("AUT", "BEL", "CHE", "DNK", "FIN", "FRA", "NOR")

states_to_sel <- c(Cl1,Cl2,Cl3,Cl4,Cl5)

############################Some preprocessing steps ############################
#Filter data
dat <- dat %>% filter(id %in% states_to_sel)

#Add Clusters variables
dat$Clusters <- ifelse(dat$id %in% Cl1, "Cl1", 
                        ifelse(dat$id %in% Cl2, "Cl2", 
                               ifelse(dat$id %in% Cl3, "Cl3", 
                                      ifelse(dat$id %in% Cl4, "Cl4",     
                                             "Cl5"))))
dat$Clusters <- factor(dat$Clusters)

#Aligned data by first confirmed case
dat <- dat %>%
  left_join(dat %>%
              filter(confirmed >= 1) %>%
              group_by(id) %>%
              summarise(date.start=min(date)),
            by="id") %>%
  mutate(date2=date-date.start) %>%
  as.data.frame()

#Compute R0 index

dat <- add_R0_ML(dat)

#Create confirmed variable lagged by 14 days
dat <- 
  dat %>% arrange(id, date) %>%
  group_by(id) %>%
  mutate(confirmed_lag = Hmisc::Lag(confirmed, shift  = -14))

#Create confirmed count variable 
dat$confirmed_count <- ave(dat$confirmed, dat$id, FUN=function(x) pmax(0, c(0, diff(x))))
dat$deaths_count <- ave(dat$deaths, dat$id, FUN=function(x) pmax(0, c(0, diff(x))))
dat$recovered_count <- ave(dat$recovered, dat$id, FUN=function(x) pmax(0, c(0, diff(x))))

#Create confirmed prop variable lagged by 14 days and active
dat <- 
  dat %>% 
  #  group_by(id) %>%
  mutate(confirmed_prop = confirmed_lag/pop,
         active = pmax(confirmed - deaths - recovered, 0),
         active_count = pmax(confirmed_count - deaths_count - recovered_count, 0))

#and lagged by 14 days
dat <- 
  dat %>% arrange(id, date) %>%
  group_by(id) %>%
  mutate(confirmed_count_lag = Hmisc::Lag(confirmed_count, shift  = -14),
         active_lag = Hmisc::Lag(active, shift  = -14),
         active_count_lag = Hmisc::Lag(active, shift  = -14))

#Transform E1 and E2 as continuous variables
pca_EC <- polychoric(dat[,var_EC[1:2]])
matPCA_ec <- cbind(rep(0,2),pca_EC$tau)
dat$E1_Income_support_f <- as.factor(dat$E1_Income_support)
dat$E1_Income_support_f <- recode_factor(dat$E1_Income_support_f,
                                                  "0" = paste0(matPCA_ec[1,1]),
                                                  "1" = paste0(matPCA_ec[1,2]),
                                                  "2" = paste0(matPCA_ec[1,3]))

dat$E1_Income_support_f <- as.numeric(dat$E1_Income_support_f)
dat$E2_Debt_contract_relief_f <- as.factor(dat$E2_Debt_contract_relief)
dat$E2_Debt_contract_relief_f <- recode_factor(dat$E2_Debt_contract_relief_f,
                                                        "0" = paste0(matPCA_ec[2,1]),
                                                        "1" = paste0(matPCA_ec[2,2]),
                                                        "2" = paste0(matPCA_ec[2,3]))
dat$E2_Debt_contract_relief_f <- as.numeric(dat$E2_Debt_contract_relief_f)

#Transform E3 and E4 in logarithmic scale
dat$E3_Fiscal_measures_log <- log(dat$E3_Fiscal_measures +1)
dat$E4_International_support <- ifelse(is.na(dat$E4_International_support), 0, dat$E4_International_support)
dat$E4_International_support_log <- log(dat$E4_International_support +1)

#Transform pop, pop density, gdp in logarithmic scale
dat$pop_log <- log(dat$pop +1)
dat$pop_density_log <- log(dat$pop_density + 1)
dat$gdp_log <- log(dat$gdp + 1)
dat$pop_urban_log<- log(dat$pop_urban + 1)
dat$surface_area_log <- log(dat$surface_area + 1)
#Pca for the two health variables
pca_hs <- princomp(na.omit(dat[,var_HS]), cor = TRUE)
dat$pca_hs <- predict(pca_hs,newdata = dat[,var_HS])[,1]

#Transform policy variables in factor
dat$school_closingF <- as.factor(dat$school_closing)
dat$cancel_eventsF <- as.factor(dat$cancel_events)
dat$gatherings_restrictionsF <- as.factor(dat$gatherings_restrictions)
dat$transport_closingF <- as.factor(dat$transport_closing)
dat$stay_home_restrictionsF <- as.factor(dat$stay_home_restrictions)
dat$workplace_closingF <- as.factor(dat$workplace_closing)
dat$internal_movement_restrictionsF <- as.factor(dat$internal_movement_restrictions)
dat$testing_policyF <- as.factor(dat$testing_policy)
dat$contact_tracingF <- as.factor(dat$contact_tracing)

#PCA policies lockdown
policies <- c("school_closing", 
              "workplace_closing", 
              "cancel_events", 
              "gatherings_restrictions",
              "transport_closing", 
              "stay_home_restrictions", 
              "internal_movement_restrictions")
W <- polychoric(dat[,policies])
p3 <- principal(r = W$rho, nfactors = 6) 
scores <- as.matrix(dat[,policies]) %*% p3$weights
dat$pca_LD <- scores[,1]


#PCA economics
policiesEC <- c("E1_Income_support", "E2_Debt_contract_relief")
W <- polychoric(dat[,policiesEC])
p3 <- principal(r = W$rho, nfactors = 1) 
db <- data.frame(dat[,policiesEC])
db$E1_Income_support <- as.numeric(db$E1_Income_support)
db$E2_Debt_contract_relief <- as.numeric(db$E2_Debt_contract_relief)
scores <- as.matrix(db) %*% p3$weights
dat$pca_EC <- scores[,1]


#Transform name variables

var_EC <- c("E1_Income_support_f","E2_Debt.contract_relief_f", "pca_EC", "E3_Fiscal_measures_log", "E4_International_support_log")

var_FIX <- c("pop_log", "pop_65", "pop_density_log", "hosp_beds", "pop_death_rate",
             "gdp_log", "pop_urban_log", "surface_area_log")

var_HS <- "pca_hs"

var_LD <- c("school_closingF","workplace_closingF","cancel_eventsF",
            "gatherings_restrictionsF","transport_closingF","stay_home_restrictionsF",
            "internal_movement_restrictionsF","testing_policyF","contact_tracingF")

#model

f <- as.formula(paste("active_lag", "~", 
                      paste(c(var_EC[c(3)]), collapse=" + "), 
                      "+", paste(c(var_FIX[c(3)]), collapse=" + "),
                      "+", paste(c(var_HS[1]), collapse=" + "),
                      "+", paste(c(var_LD[c(2,4:6,8:9)]),collapse=" + "),
                      "+ Clusters + (0 + pca_LD|id) + (1|date2)"))

mod1 <- glmmTMB(f, dat, family="nbinom2", offset = log(pop + 1))

comp_cluster <- glht_glmmTMB(mod1, linfct = mcp(Clusters = "Tukey"))

comp_work <- glht_glmmTMB(mod1, linfct = mcp(workplace_closingF = "Tukey"))

save(list = ls(all.names = TRUE), file = paste0(path,"Angela/out.RData"))

save(list = ls(all.names = TRUE), file = "out.RData")

