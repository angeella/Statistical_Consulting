#################################MIXED MODEL######################################

#We need to take into account the nested and repeated measures structures:
#Cluster -> ID -> Time
#having covariates that change between time and covariates that not.

#load package
source("Angela/packages.R")

#load data
source("Angela/loadData.R")

#Mixed model = https://rpsychologist.com/r-guide-longitudinal-lme-lmer
####Conditional three-level growth model#####
#It expands to a main effect of Clusters and a interaction 
#between Clusters and id (which is the id level effect).
heatmap(cor(datA[,c(var_EC,var_FIX,var_LD,var_MOB,"tests")],use = "na.or.complete"))

f <- as.formula(paste("R0mean", "~", paste(c(var_EC), "date", collapse=" + ",sep = "*"), 
                      "+", paste(c(var_FIX), collapse=" + "),
                      "+", paste(c(var_HS), "date", collapse=" + ",sep = "*"),
                      "+", paste(c(var_LD), "date", collapse=" + ",sep = "*"),
                      "+", paste(c(var_MOB), "date", collapse=" + ",sep = "*"),
                      "+", "tests*date + date",
                      "+ (1+date|Clusters/id)"))

model <- lmer(f, data = datA,REML = TRUE)
isSingular(model)
summary(model)
coefplot2::coefplot2(model)
ranef(model)

corrgram(datA[,var_EC])
cor(datA[,var_EC],use = "na.or.complete")

#To analyze:
#Income.support VS Debt.Contract.relief
table(datA$ox.E1_Income.support,exclude = FALSE)
table(datA$ox.E2_Debt.contract.relief, exclude = FALSE)
#I drop off ox.E2_Debt.contract.relief having more NAs.

corrgram(datA[,var_FIX])
cor(datA[,var_FIX],use = "na.or.complete")
#gdp o pop!
#pop_age o pop_65!
#pop_death_rate or hosp_beds

summary(datA$gdp)
summary(datA$pop)
#Drop off gdp! I will mantains the other for now.

corrgram(datA[,var_LD])
cor(datA[,var_LD],use = "na.or.complete")

var_FIX <- var_FIX[1:6]
var_EC <- var_EC[2:6]

#meglio usare le tre componenti principali fatte da Silvia invece che tutte

policies <- c(var_LD, var_HS[1:3])
var_HS <- var_HS[4:6]

W <- polychoric(datA[,policies])
p3 <- principal(r = W$rho, nfactors = 6) 
cbind(policies, p3$loadings)

#1a restrictions pol 
#2a contact tracing +info+testing 
#3a eco policies

p3$scores <- factor.scores(sapply(datA[,policies], as.numeric),p3)
PCpolicies=p3$scores$scores[,1:3]
colnames(PCpolicies)=c("restrinctions", "tracing", "testing")

dim(PCpolicies)
dim(datA)
datAA <- cbind(datA, PCpolicies)

#Mixed model
f <- as.formula(paste("R0mean", "~", paste(c(var_EC), "date", collapse=" + ",sep = "*"), 
                      "+", paste(c(var_FIX), collapse=" + "),
                      "+", paste(c(var_HS), "date", collapse=" + ",sep = "*"),
                     "+", paste(colnames(PCpolicies), "date", collapse=" + ",sep = "*"),
                      "+", paste(c(var_MOB), "date", collapse=" + ",sep = "*"),
                      "+", "tests*date + date",
                      "+ (date|Clusters/id)"))


model <- lmer(f, data = datAA)
isSingular(model)
summary(model)

corrgram(datAA[,var_MOB])
cor(datAA[,var_MOB],use = "na.or.complete")

#PCA Mobility data

datAAA <- datAA

datAAA[,var_MOB] <- sapply(var_MOB, function(x) as.numeric(datAAA[,x]))
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
datAAA[,var_MOB] <- lapply(datAAA[,var_MOB] , NA2mean)
mob_comp <- prcomp(datAAA[,var_MOB], scale. = TRUE)
summary(mob_comp)

rownames(mob_comp[[2]]) <- c("recreation", "shop", "park", "station", "work", "residential")

datAA$Clusters <- as.factor(datAA$Clusters)

ggbiplot(mob_comp, groups = datAA$Clusters, circle = TRUE,ellipse=TRUE)

#PC1 residentail positive and everything negative, ma un po' tutto
#PC2 park! principlamente
#PC3 shop!

#mmm non so comunque come prima analisi uso queste
head(mob_comp$x <- as.data.frame(mob_comp$x))

datAA <- cbind(datAA, mob_comp$x[1:3])
colnames(datAA)[131:133] <- c("res", "park", "shop")

var_MOB2 <- c("res", "park", "shop")


ICCest(Clusters, R0mean, datAA) #-0.0002235652
ICCest(id, R0mean, datAA) #0.02130633

#PLOT!!!
abc<-aggregate(R0mean ~Clusters, datAA, mean)
bdata <- datAA
bdata <- merge(bdata,abc,by="Clusters")
bdata$colorBox <- ifelse(bdata$R0mean.y>= mean(bdata$R0mean.x,na.rm = TRUE), "#56B4E9", "#009E73")

ggplot(bdata, aes(x=Clusters, y=R0mean.x, fill = colorBox)) +
  geom_boxplot() +
  scale_color_manual(values=rainbow(6))+ theme(legend.position="none",axis.title.x=element_blank(),
                                               axis.text.x=element_blank(),
                                               axis.ticks.x=element_blank())

abc<-aggregate(R0mean ~id, datAA, mean)
bdata <- datAA
bdata <- merge(bdata,abc,by="id")
bdata$colorBox <- ifelse(bdata$R0mean.y>= mean(bdata$R0mean.x,na.rm = TRUE), "#56B4E9", "#009E73")

ggplot(bdata, aes(x=id, y=R0mean.x, fill = colorBox)) +
  geom_boxplot() +
  scale_color_manual(values=rainbow(6))+ theme(legend.position="none",axis.title.x=element_blank(),
                                               axis.text.x=element_blank(),
                                               axis.ticks.x=element_blank())


df <- as.data.frame(aggregate(R0mean ~Clusters, datAA, length)) %>% arrange(desc(R0mean))
ggplot(datAA[(datAA$Clusters %in% df$Clusters),], aes(x = restrinctions, y = R0mean, group = Clusters)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
  geom_point(alpha = 0.3, size = 3,color="blue") +
  facet_wrap(~Clusters) +
  theme_minimal()

df <- as.data.frame(aggregate(R0mean ~id, datAA, length)) %>% arrange(desc(R0mean))
ggplot(datAA[(datAA$id %in% df$id),], aes(x = restrinctions, y = R0mean, group = id)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
  geom_point(alpha = 0.3, size = 3,color="blue") +
  facet_wrap(~id) +
  theme_minimal()

#Mixed model
f <- as.formula(paste("R0mean", "~", paste(c(var_EC), "date", collapse=" + ",sep = ":"), 
                      "+", paste(c(var_FIX), collapse=" + "),
                      "+", paste(c(var_HS), "date", collapse=" + ",sep = ":"),
                      "+", paste(colnames(PCpolicies), "date", collapse=" + ",sep = ":"),
                      "+", paste(c(var_MOB2), "date", collapse=" + ",sep = ":"),
                      "+", "tests:date",
                      "+ (date|Clusters/id)"))


model <- lmer(f, data = datAA)
isSingular(model)
round(summary(model)$coefficients,5)
#We eliminate pop, and we mantain pop_density. We eliminate:
# ox.E2_Debt.contract.relief:date                    0.000      0.000  -2.786
# date:ox.E3_Fiscal.measures                         0.000      0.000   0.287
# date:ox.E4_International.support                   0.000      0.000   0.004
# date:mkt_close                                     0.000      0.000  -1.044
# date:mkt_volume                                    0.000      0.000   6.802
# date:ox.H4_Emergency.investment.in.healthcare      0.000      0.000   0.585
# date:ox.H5_Investment.in.vaccines                  0.000      0.000   0.391

#Doubts: the clusters are not randomly selected -> fixed effects??

f2 <- as.formula(paste("R0mean", "~", 
                        paste(c(var_EC), "date", collapse=" + ",sep = ":"), 
                       "+", paste(c(var_FIX[2:6]),collapse=" + "),
                        "+", paste(c(var_HS), "date", collapse=" + ",sep = ":"),
                       "+", paste(colnames(PCpolicies), "date", collapse=" + ",sep = ":"),
                       "+", paste(c(var_MOB2), "date",collapse=" + ",sep = ":"),
                       "+", "tests:date","+ Clusters",
                       "+ (1 + date|id)"))

model2 <- lmer(f2, data = datAA)
summary(model2)

tmp <- as.data.frame(confint(glht(model2, mcp(Clusters = "Tukey")))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) +
  geom_errorbar() + geom_point()

#Simple model

f3 <- as.formula(paste("R0mean", "~", 
                       paste(c(var_EC),collapse=" + "), 
                       "+", paste(c(var_FIX), collapse=" + "),
                      "+", paste(c(var_HS), collapse=" + "),
                       "+", paste(colnames(PCpolicies), collapse=" + "),
                       "+", paste(c(var_MOB2), collapse=" + "),
                       "+", "tests",
                       "+ (date|Clusters)"))

model3 <- lmer(f3, data = datAA)
summary(model3)

var_exp <- c(var_EC,var_HS,colnames(PCpolicies),var_MOB2, "tests", var_FIX)
heatmap(cor(datAA[,var_exp],use = "na.or.complete"))

summary(datAA$tests)
summary(datAA$restrinctions)
summary(datAA$ox.E2_Debt.contract.relief)
summary(datAA$res)

#drop ox.E2_Debt.contract.relief

summary(datAA$mkt_volume)
summary(datAA$testing)
summary(datAA$pop_density)

#per ora tengo, altrimenti tolgo mkt_volume


summary(datAA$mkt_close)
summary(datAA$pop)
summary(datAA$park)

#tolgo mkt_close
summary(datAA$pop_age)
summary(datAA$pop_65)
summary(datAA$pop_death_rate)
summary(datAA$hosp_beds)
summary(datAA$health_exp)

#tolgo pop_65, pop_death_rate, health_exp

new_var <- var_exp[!(var_exp %in% c("ox.E2_Debt.contract.relief", 
               "mkt_close",
               "pop_65",
               "pop_death_rate",
               "health_exp"))]

heatmap(cor(datAA[,new_var],use = "na.or.complete"))

f4 <- as.formula(paste("R0mean", "~", 
                       paste(c(new_var),collapse=" + "), 
                       "+ (date|Clusters/id)"))

model4 <- lmer(f4, data = datAA)
summary(model4)
coplot(R0mean~date|Clusters, type="b", data=datAA) # Points and lines
scatterplot(R0mean~date|Clusters, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=datAA)

gplots::plotmeans(R0mean ~ Clusters, main="Heterogeineity across countries", data=datAA)
gplots::plotmeans(R0mean ~ date, main="Heterogeineity across time", data=datAA)

#Return to simple model
library(lmerTest)

model = lmer(R0mean ~ tests +  (1+date|Clusters),
             data=datAA,
             REML=TRUE)

rand(model)

#ok add var


f3 <- as.formula(paste("R0mean", "~", 
                       "+", paste(c(nnew_var), collapse=" + "),
                       "+", "tests",
                       "+ (1+date|Clusters)"))

model = lmer(f3,
             data=datAA,
             REML=TRUE)

rand(model)
summary(model)
#ox.E3_Fiscal.measures                     2.460e-12  2.630e-12  1.011e+03   0.935  0.34992    
#ox.E4_International.support              -9.434e-11  1.294e-09  1.011e+03  -0.073  0.94188
#pop                                      -3.005e-09  5.644e-09  1.009e+03  -0.532  0.59461
#ox.H4_Emergency.investment.in.healthcare  4.250e-10  3.812e-10  1.011e+03   1.115  0.26515    
#ox.H5_Investment.in.vaccines              1.751e-09  6.493e-09  1.011e+03   0.270  0.78743

f <- as.formula(paste("R0mean", "~", 
                       paste(c(var_EC[2]),collapse=" + "), 
                       "+", paste(c(var_FIX[c(2,5)]), collapse=" + "),
                       "+", paste(c(var_HS[3]), collapse=" + "),
                      # "+", paste(colnames(PCpolicies), collapse=" + "),
                      # "+", paste(c(var_MOB2[1:2]), collapse=" + "),
                      # "+", "tests",
                       "+ (date|Clusters)"))
model = lmer(f,
             data=datAA,
             REML=TRUE)
rand(model)
summary(model)

#Questo sembra ragionevole
#ora voglio fare dei grafici per ogni variabile di gruppo
#pca per ogni gruppo di var che ho preso in esame

#Iniziamo con i grafici
ggplot(datAA,aes(x=ox.E3_Fiscal.measures))+geom_boxplot()+facet_grid(~Clusters)+theme_bw()
#fa un po' schifo, rivedo le variabili economiche
var_EC
ggplot(datAA,aes(x=ox.E2_Debt.contract.relief))+geom_boxplot()+facet_grid(~Clusters)+theme_bw()
ggplot(datAA,aes(x=ox.E4_International.support))+geom_boxplot()+facet_grid(~Clusters)+theme_bw()
ggplot(datAA,aes(x=mkt_close))+geom_boxplot()+facet_grid(~Clusters)+theme_bw()
ggplot(datAA,aes(x=mkt_volume))+geom_boxplot()+facet_grid(~Clusters)+theme_bw()

#io riproverei con ox.E2_Debt contract relief
#provo la pca:

datAA[,var_EC] <- sapply(var_EC, function(x) as.numeric(datAA[,x]))
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
datAA[,var_EC] <- lapply(datAA[,var_EC] , NA2mean)
ec_comp <- prcomp(datAA[,var_EC], scale = TRUE, center = TRUE)
summary(ec_comp) 
ggbiplot(ec_comp, groups = datAA$Clusters, circle = TRUE,ellipse=TRUE)
ec_comp$rotation
#bah poco niente....

ggplot(datAA,aes(x=pop))+geom_boxplot()+facet_grid(~Clusters)+theme_bw()
ggplot(datAA,aes(x=pop_65))+geom_boxplot()+facet_grid(~Clusters)+theme_bw()
ggplot(datAA,aes(x=pop_age))+geom_boxplot()+facet_grid(~Clusters)+theme_bw()
ggplot(datAA,aes(x=pop_density))+geom_boxplot()+facet_grid(~Clusters)+theme_bw()
ggplot(datAA,aes(x=hosp_beds))+geom_boxplot()+facet_grid(~Clusters)+theme_bw()
ggplot(datAA,aes(x=pop_death_rate))+geom_boxplot()+facet_grid(~Clusters)+theme_bw()

#piu o meno pop_65 e pop_age uguali
#io terrei pop_65 e hosp_beds.
datAA[,var_FIX] <- sapply(var_FIX, function(x) as.numeric(datAA[,x]))
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
datAA[,var_FIX] <- lapply(datAA[,var_FIX] , NA2mean)
fix_comp <- prcomp(datAA[,var_FIX], scale = TRUE, center = TRUE)
summary(fix_comp) 
ggbiplot(fix_comp, groups = datAA$Clusters, circle = TRUE,ellipse=TRUE)
fix_comp$rotation
heatmap(cor(datAA[,var_FIX],use = "na.or.complete"))

ggplot(datAA,aes(x=ox.H4_Emergency.investment.in.healthcare))+geom_boxplot()+facet_grid(~Clusters)+theme_bw()
ggplot(datAA,aes(x=ox.H5_Investment.in.vaccines))+geom_boxplot()+facet_grid(~Clusters)+theme_bw()
ggplot(datAA,aes(x=health_exp))+geom_boxplot()+facet_grid(~Clusters)+theme_bw()
#terrei health exp

datAA[,var_HS] <- sapply(var_HS, function(x) as.numeric(datAA[,x]))
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
datAA[,var_HS] <- lapply(datAA[,var_HS] , NA2mean)
hs_comp <- prcomp(datAA[,var_HS], scale = TRUE, center = TRUE)
summary(hs_comp) 
ggbiplot(hs_comp, groups = datAA$Clusters, circle = TRUE,ellipse=TRUE)
hs_comp$rotation
heatmap(cor(datAA[,var_HS],use = "na.or.complete"))
#anche se sarebbero da tenere tutte


ggplot(datAA,aes(x=res))+geom_boxplot()+facet_grid(~Clusters)+theme_bw()
ggplot(datAA,aes(x=park))+geom_boxplot()+facet_grid(~Clusters)+theme_bw()
ggplot(datAA,aes(x=shop))+geom_boxplot()+facet_grid(~Clusters)+theme_bw()
#terrei res

ggplot(datAA,aes(x=tests))+geom_boxplot()+facet_grid(~Clusters)+theme_bw()

ggplot(datAA,aes(x=restrinctions))+geom_boxplot()+facet_grid(~Clusters)+theme_bw()
ggplot(datAA,aes(x=tracing))+geom_boxplot()+facet_grid(~Clusters)+theme_bw()
ggplot(datAA,aes(x=testing))+geom_boxplot()+facet_grid(~Clusters)+theme_bw()

f <- as.formula(paste("R0mean", "~", 
                      paste(c(var_EC[1]),collapse=" + "), 
                      "+", paste(c(var_FIX[c(2,5)]), collapse=" + "),
                     # "+", paste(c(var_HS[3]), collapse=" + "),
                      # "+", paste(colnames(PCpolicies), collapse=" + "),
                       "+", paste(c(var_MOB2[1]), collapse=" + "),
                       "+", "tests",
                      "+ (date|Clusters/id)"))
model = lmer(f,
             data=datAA,
             REML=TRUE)
rand(model)
summary(model)

var_tot <- c(var_EC, var_FIX, var_HS, var_MOB2, "tests")
label_var_tot <- var_tot
label_var_tot[1] <- "Debt_contract_relief"
label_var_tot[2] <- "Fiscal_measures"
label_var_tot[3] <- "International_support"
label_var_tot[12] <- "Emergency_inv_healthcare"
label_var_tot[13] <- "Inv_vaccines"
heatmap(cor(datAA[,var_final],
            use = "na.or.complete"),
        labRow = label_var_tot)

#-0.75 correlation
coefplot2::coefplot2(model)
ranef(model)

ggplot(datAA,aes(x=R0mean, fill = Clusters))+geom_boxplot()
ggplot(datAA,aes(x=R0mean, fill = id))+geom_boxplot()

ggplot(datAA,aes(x=date,y = R0mean, fill = Clusters, col = Clusters))+geom_point()
ggplot(datAA,aes(x=date, y = R0mean,fill = id, col = id))+geom_line()

#Se uso Clusters come fixed?
  
f <- as.formula(paste("R0mean", "~", 
                      paste(c(var_EC[1]),collapse=" + "),
                      "+", paste(c(var_FIX[c(2,5)]),collapse=" + "),
                      "+", paste(c(var_HS[3]), collapse=" + "),
                      # "+", paste(colnames(PCpolicies), collapse=" + "),
                      "+", paste(c(var_MOB[1]), collapse=" + "),
                      "+", "tests + Clusters",
                      "+ (date|id)"))
model = lmer(f,
             data=datAA,
             REML=TRUE)
summary(model)

coefplot2::coefplot2(model)
tmp <- as.data.frame(confint(glht(model, mcp(Clusters = "Tukey")))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) +
  geom_errorbar() + geom_point()




