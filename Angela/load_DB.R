#load package
setwd("~/GitHub/Statistical_Consulting/Angela/")

source("packages.R")

#load variables
load("Data/var.RData")

#load data
load("Data/db_with_R0.RData")

dat <- df
#add Cluster Silvia aligned

uno <- c("KOR", "SGP")
due <- c("DEU", "SWE")
tre <- c("CAN", "GRC", "PRT","USA")
quattro <- c("ESP", "GBR", "IRL", "ITA", "NLD")
cinque <- c("AUT", "BEL", "CHE", "DNK", "FIN", "FRA", "NOR")
states_to_sel <- c(uno,due,tre,quattro,cinque)

datA <- dat %>% filter(id %in% states_to_sel)

datA$Clusters <- ifelse(datA$id %in% uno, "Cl1", 
                        ifelse(datA$id %in% due, "Cl2", 
                               ifelse(datA$id %in% tre, "Cl3", 
                                      ifelse(datA$id %in% quattro, "Cl4", 
                                             "Cl5"))))
dat <- datA

#Some steps 
dat$Clusters <- factor(dat$Clusters)

dat_shape <- dat %>%
  left_join(dat %>%
              filter(confirmed >= 1) %>%
              group_by(id) %>%
              summarise(date.start=min(date)),
            by="id") %>%
  mutate(date2=date-date.start) %>%
  as.data.frame()

dat_shape <- add_R0_ML(dat_shape)

dat_shape <- 
  dat_shape %>% arrange(id, date) %>%
  group_by(id) %>%
  mutate(confirmed_lag = Hmisc::Lag(confirmed, shift  = -14))

dat_shape <- 
  dat_shape %>% 
  #  group_by(id) %>%
  mutate(confirmed_prop = confirmed_lag/pop,
         active = pmax(confirmed - deaths - recovered, 0))

dat_shape$confirmed_count <- ave(dat_shape$confirmed, dat_shape$id, FUN=function(x) pmax(0, c(0, diff(x))))

dat_shape <- 
  dat_shape %>% arrange(id, date) %>%
  group_by(id) %>%
  mutate(confirmed_count_lag = Hmisc::Lag(confirmed_count, shift  = -14),
         active_lage = Hmisc::Lag(active, shift  = -14))


pca_EC <- polychoric(dat_shape[,var_EC[1:2]])
matPCA_ec <- cbind(rep(0,2),pca_EC$tau)
dat_shape$ox.E1_Income.support_f <- as.factor(dat_shape$ox.E1_Income.support)
dat_shape$ox.E1_Income.support_f <- recode_factor(dat_shape$ox.E1_Income.support_f,
                                                  "0" = paste0(matPCA_ec[1,1]),
                                                  "1" = paste0(matPCA_ec[1,2]),
                                                  "2" = paste0(matPCA_ec[1,3]))

dat_shape$ox.E1_Income.support_f <- as.numeric(dat_shape$ox.E1_Income.support_f)
dat_shape$ox.E2_Debt.contract.relief_f <- as.factor(dat_shape$ox.E2_Debt.contract.relief)
dat_shape$ox.E2_Debt.contract.relief_f <- recode_factor(dat_shape$ox.E2_Debt.contract.relief_f,
                                                        "0" = paste0(matPCA_ec[2,1]),
                                                        "1" = paste0(matPCA_ec[2,2]),
                                                        "2" = paste0(matPCA_ec[2,3]))
dat_shape$ox.E2_Debt.contract.relief_f <- as.numeric(dat_shape$ox.E2_Debt.contract.relief_f)
dat_shape$ox.E3_Fiscal.measures_log <- log(dat_shape$ox.E3_Fiscal.measures +1)
dat_shape$ox.E4_International.support <- ifelse(is.na(dat$ox.E4_International.support), 0, dat_shape$ox.E4_International.support)
dat_shape$ox.E4_International.support_log <- log(dat_shape$ox.E4_International.support +1)
dat_shape$pop_log <- log(dat_shape$pop +1)
pca_age <- princomp(na.omit(dat_shape[,var_FIX[2:3]]), cor = TRUE)
dat_shape$pca_age <- predict(pca_age,newdata = dat_shape[,var_FIX[2:3]])[,1]
dat_shape$pop_density_log <- log(dat_shape$pop_density + 1)
dat_shape$pop_death_rate_1000 <- dat_shape$pop_death_rate * 1000
dat_shape$gdp_log <- log(dat_shape$gdp + 1)
pca_hs <- princomp(na.omit(dat_shape[,var_HS]), cor = TRUE)
dat_shape$pca_hs <- predict(pca_hs,newdata = dat_shape[,var_HS])[,1]
dat_shape$school_closingF <- as.factor(dat_shape$school_closing)
dat_shape$cancel_eventsF <- as.factor(dat_shape$cancel_events)
dat_shape$gatherings_restrictionsF <- as.factor(dat_shape$gatherings_restrictions)
dat_shape$transport_closingF <- as.factor(dat_shape$transport_closing)
dat_shape$stay_home_restrictionsF <- as.factor(dat_shape$stay_home_restrictions)
dat_shape$workplace_closingF <- as.factor(dat_shape$workplace_closing)
dat_shape$internal_movement_restrictionsF <- as.factor(dat_shape$internal_movement_restrictions)
dat_shape$testing_policyF <- as.factor(dat_shape$testing_policy)
dat_shape$contact_tracingF <- as.factor(dat_shape$contact_tracing)

#pca policies

policies <- c("school_closing", "workplace_closing", "cancel_events", "gatherings_restrictions","transport_closing", "stay_home_restrictions", "internal_movement_restrictions")
W <- polychoric(dat_shape[,policies])
W$tau
p3 <- principal(r = W$rho, nfactors = 6) 
p3$loadings #prime 3 componenti
cbind(policies, p3$loadings)

scores <- as.matrix(dat_shape[,policies]) %*% p3$weights
dat_shape$pca_LD <- scores[,1]

#pca economics
policiesEC <- c("ox.E1_Income.support", "ox.E2_Debt.contract.relief")

W <- polychoric(dat_shape[,policiesEC])
W$tau
p3 <- principal(r = W$rho, nfactors = 1) 
p3$loadings #prime 3 componenti
cbind(policies, p3$loadings)

scores <- as.matrix(dat_shape[,policiesEC]) %*% p3$weights
dat_shape$pca_EC <- scores[,1]


var_EC <- c("ox.E1_Income.support_f","ox.E2_Debt.contract.relief_f", var_EC[5:6], "ox.E3_Fiscal.measures_log", "ox.E4_International.support_log")

var_FIX <- c("pca_age", "pop_log", "pop_density_log", "hosp_beds", "pop_death_rate_1000", "gdp_log", "health_exp")

var_HS <- "pca_hs"

var_LD <- c("school_closingF","workplace_closingF","cancel_eventsF",
            "gatherings_restrictionsF","transport_closingF","stay_home_restrictionsF",
            "internal_movement_restrictionsF","testing_policyF","contact_tracingF")

f <- as.formula(paste("active_lage", "~", 
                      #   paste(c(var_EC[c(3)]), collapse=" + "), 
                      #   "+", paste(c(var_FIX[c(2,3)]), collapse=" + "),
                      #    "+", paste(c(var_HS[1]), collapse=" + "),
                      # "+ log(ox.H4_Emergency.investment.in.healthcare +1)",
                      "+", paste(c(var_LD[c(1,4, 5,6:9)]),collapse=" + "),
                      "+ Clusters  + pop_density_log + pca_EC + (0 + pca_LD|id) + (1|date2) + (1| Clusters)"))

mod1 <- glmmTMB(f, dat_shape, family="nbinom2", offset = log(active + 1))

save(list = ls(all.names = TRUE), file = "out.RData")
