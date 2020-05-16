#New analysis

#load package
source("Angela/packages.R")

#load variables
load("Angela/Data/var.RData")

#load data
#source("Angela/loadData.R")

load("Data/db_with_R0.RData")
policies <- c("school_closing","workplace_closing","cancel_events", "gatherings_restrictions",
              "transport_closing","stay_home_restrictions","internal_movement_restrictions",
              "international_movement_restrictions","information_campaigns","testing_policy","contact_tracing")
#add R0 ML
dat <- add_R0_ML(df)
colnames(dat)

#load cluster vecchio
uno <-as.character(readRDS("Silvia/uno.rds"))
due <-as.character(readRDS("Silvia/due.rds"))
tre <-as.character(readRDS("Silvia/tre.rds"))

states_to_sel <- unique(c(uno,due,tre))
length(states_to_sel) #27

datA <- dat %>% filter(id %in% states_to_sel)
dim(datA)

datA$Clusters <- ifelse(datA$id %in% uno, 1, 
                        ifelse(datA$id %in% due, 2, 3))
dat <- datA

#Cluster align
datMIO <- dat
load("Silvia/clusterswithalign.Rdata")
states_to_sel <- levels(uno)
length(states_to_sel) #28
dat <- datMIO
datA <- dat %>% filter(id %in% states_to_sel)
dim(datA)


datA$Clusters <- ifelse(datA$id %in% uno, "Cl1", 
                        ifelse(datA$id %in% due | datA$id %in% sei, "Cl2", 
                               ifelse(datA$id %in% tre, "Cl3", 
                                      ifelse(datA$id %in% quattro, "Cl4", 
                                             "Cl5"))))

dat <- datA
#Manage date
dat$dateF <- factor(dat$date)
dat$Clusters <- factor(dat$Clusters)
#levels(dat$Clusters) <- c("Cl1", "Cl2", "Cl3")

#dat$Clusters <-recode_factor(dat$Clusters, "1" = "Cl1", "2" = "Cl2", "3" = "Cl3")

#graph
dat %>% ggplot(aes(x=date, y=R0, group=Clusters, color=Clusters)) + 
  geom_point(size=1) + theme_classic()

dat %>% ggplot(aes(x=date, y=R0, group=Clusters, color=Clusters)) + 
  geom_smooth()

datAA <- dat %>% filter(date >= as.Date("2020-15-02", format= "%Y-%d-%m"))

datAA %>% ggplot(aes(x=date, y=R0, group=Clusters, color=Clusters)) + 
  geom_smooth()
#Three times: 15 Febr , 1 Apr
dat$EPOCH <- ifelse(dat$date <= "2020-03-1", "First",
                    ifelse(dat$date <= "2020-04-01", "Second", "Third"))

ggplot(dat, aes(x=EPOCH, y=R0, fill=EPOCH)) + geom_boxplot() 

dat <- 
  dat %>% arrange(id, date) %>%
  group_by(id) %>%
  mutate(R0_lag = dplyr::lag(R0, n = 14, default = NA))

#var_EC
#ox.E1_Income Support ordinal
#ox.E2_Debt.contract.relief ordinal
#ox.E3_Fiscal.measures ordinal
#ox.E4_International.support ordinal
#mkt_close and volume, very high. scale

dat <- 
  dat %>%
  mutate(mkt_close_S = log(mkt_close+1),
         mkt_volume_S = log(mkt_volume+1),
         ox.E4_International.support_S = log(ox.E4_International.support+1),
         ox.E3_Fiscal.measures_S = log(ox.E3_Fiscal.measures+1))

var_EC_transformed <- c("ox.E1_Income.support", "ox.E2_Debt.contract.relief", "ox.E3_Fiscal.measures_S",
                        "ox.E4_International.support_S", "mkt_close_S", "mkt_volume_S")

#var_FIX
#pop scale
#gdp scale
#pop density scale

dat$pop_density_S <- (dat$pop_density - mean(dat$pop_density,na.rm = TRUE)) / sd(dat$pop_density,na.rm = TRUE)
dat$gdp_S <- (dat$gdp - mean(dat$gdp,na.rm = TRUE)) / sd(dat$gdp,na.rm = TRUE)
dat$pop_S <- (dat$pop - mean(dat$pop,na.rm = TRUE)) / sd(dat$pop,na.rm = TRUE)

var_FIX_transformed <- c("pop_S", "gdp_S", "pop_density_S",
                        "pop_65", "pop_age", "hosp_beds", "pop_death_rate")

dat$EPOCH <- as.factor(dat$EPOCH)

C <-cor(dat[,c(var_EC_transformed,var_FIX_transformed,var_HS,"tests", "R0_lag")],use = "na.or.complete")

heatmap(C)
#pop_age nor pop_65
#mkt_volume not close
#health_exp not pop_density (less NA)
#ox.E2_Debt.contract.relief not ox.incomesupport
#drop gdp

f <- as.formula(paste("R0_lag", "~", paste(c(var_EC_transformed[c(2,3,4,6)]), collapse=" + "), 
                      "+", paste(c(var_FIX[c(1,5,6)]), collapse=" + "),
                      "+", paste(c(var_HS[c(4:6)]), collapse=" + "),
                      "+", "tests + Clusters1",
                      "+ (1|EPOCH)"))


f <- as.formula(paste("R0_lag", "~", paste(c(var_EC[c(1,5)]), collapse=" + "), 
                      "+", paste(c(var_FIX[c(1,5,7)]), collapse=" + "),
                      "+", paste(c(var_HS[c(5,6)]), collapse=" + "),
                      "+", "Clusters1",
                      "+ (1|EPOCH)"))

model1 <- lmer(f, data = dat,REML = TRUE)

dat$Clusters1 <- recode_factor(dat$Clusters,
                              "Cl1" = "tracing",
                              "Cl2" = "restriction_tracing",
                              "Cl3" = "testing",
                              "Cl4" = "all",
                              "Cl5" = "tracing_testing")

summary(model1)
comparison <- c("tracing - restriction_tracing = 0",
                "tracing - testing = 0",
                "tracing - all = 0",
                "tracing - tracing_testing = 0",
                "restriction_tracing - testing = 0",
                "restriction_tracing - all = 0",
                "restriction_tracing - tracing_testing = 0",
                "testing - all = 0",
                "testing - tracing_testing = 0",
                "all - tracing_testing = 0")

comparison <- c("Cl1 - Cl2 = 0",
                "Cl1 - Cl3 = 0",
                "Cl1 - Cl4 = 0",
                "Cl1 - Cl5 = 0",
                "Cl2 - Cl3 = 0",
                "Cl2 - Cl4 = 0",
                "Cl2 - Cl5 = 0",
                "Cl3 - Cl4 = 0",
                "Cl3 - Cl5 = 0",
                "Cl4 - Cl5 = 0")
test<-multcomp::glht(model1, linfct = mcp(Clusters1 = comparison),adjusted="holm")
summary(test)
ggplot(datAA, aes(x=Clusters, y=R0_lag, fill=Clusters)) + geom_boxplot() 
datAA$Clusters1 <- as.factor(datAA$Clusters1)
ggplot(datAA, aes(x=Clusters1, y=R0_lag, fill=Clusters1)) + geom_boxplot() 

eff <- data.frame(effect("Clusters1", model1))
#eff$Group <- factor(eff$Group,levels = c("cnt","fep","scz", "bipo"))

ggplot(eff, aes(x=Clusters1, y=fit)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(group = 1)) + 
  geom_point(aes(x=reorder(Clusters1, upper, desc), y=fit))+
  xlab("Clusters") + ylab("R0")

#Tutte le variabili?
f <- as.formula(paste("R0mean", "~", paste(c(var_EC), collapse=" + "), 
                      "+", paste(c(var_FIX), collapse=" + "),
                      "+", paste(c(var_HS), collapse=" + "),
                      "+", "tests + Clusters",
                      "+ (1|EPOCH)"))
model1 <- lmer(f, data = dat,REML = FALSE)

summary(model1)
comparison <- c("Restrictions - Tracing = 0",
                "Restrictions - Testing = 0",
                "Tracing - Testing = 0")

test<-multcomp::glht(model1, linfct = mcp(Clusters = comparison),adjusted="holm")
summary(test)

#Cluster tracing aumenta di piú R0 rispetto a restrictions e rispetto a testing.