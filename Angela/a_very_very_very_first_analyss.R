#Very first cluster 
library(amap)
library(ggplot2)
library(lme4)
library(multcomp)
#load variables set
load("Angela/Data/var.RData")

dataC <- dat[,c(var_EC,var_FIX,var_HS,var_LD,var_MOB, "id", "date", "confirmed")]
dim(dataC)

var_Ord <- c("ox.E1_Income.support", "ox.E2_Debt.contract.relief", "information_campaigns",
             "testing_policy", "contact_tracing","school_closing", "workplace_closing",
             "cancel_events", "gatherings_restrictions", "transport_closing", 
             "stay_home_restrictions", "internal_movement_restrictions"
             )

#d <- Dist(dataC[,var_Ord], method = "manhattan") 
#summary(d)

var_Num <- c("ox.E3_Fiscal.measures","ox.E4_International.support", "mkt_close",
             "mkt_volume", "pop", "pop_65", "pop_age" ,"pop_density", "hosp_beds",
             "pop_death_rate", "gdp", "ox.H4_Emergency.investment.in.healthcare",
             "ox.H5_Investment.in.vaccines", "health_exp", "health_exp_oop", 
             var_MOB)

d1 <- dist(scale(dataC[,c(1:33)]), method = "euclidean") 

H.fit <- hclust(d1, method="ward")
#plot(H.fit)
groups <- cutree(H.fit, k=4) # cut tree into 4 clusters
out <- cbind(groups, dataC$id)
colnames(out) <- c("groups", "id")
out <- as.data.frame(out)
groups1 <- unique(out$id[out$groups == "1"])
groups2 <- unique(out$id[out$groups == "2"])
groups3 <- unique(out$id[out$groups == "3"])
groups4 <- unique(out$id[out$groups == "4"])

a<-as.character(groups1[which(groups1==groups2)])
b<-as.character(groups1[which(groups1==groups3)])
c<-as.character(groups1[which(groups1==groups4)])
d<-as.character(groups2[which(groups2==groups3)])
e<-as.character(groups2[which(groups2==groups4)])
f<-as.character(groups3[which(groups3==groups4)])
#drop AFG, AGO, ALB, BFA, BGD BGR BHR, COD IRN IRQ ISL ISR KAZ KEN KGZ, 

dataC$CLUSTER <- groups
dataC_filter <- dataC %>% filter(!(id %in% c(unique(c(a,b,c,d,e,f)))))

dim(dataC)
dim(dataC_filter)

dat <- add_R0(dataset = dataC_filter)
library(lme4)
dat[,var_Ord] <- lapply(dat[,var_Ord], function(x) as.factor(x))
f <- as.formula(paste("R0mean", "~", paste(c(var_EC), collapse=" + "), 
                      "+", paste(c(var_FIX), collapse=" + "),
                      "+", paste(c(var_HS), collapse=" + "),
                      "+", paste(c(var_LD), collapse=" + "),
                      "+ (1|id) + (date|CLUSTER)"))
model <- lmer(f, data = dat)
tmp <- as.data.frame(confint(glht(model, mcp(stay_home_restrictions = "Tukey")))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) +
  geom_errorbar() + geom_point()

tmp <- as.data.frame(confint(glht(model, mcp(testing_policy = "Tukey")))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) +
  geom_errorbar() + geom_point()

tmp <- as.data.frame(confint(glht(model))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) +
  geom_errorbar() + geom_point()

library(plotrix)
sizetree(dat[,c(1,34)])

lattice::xyplot(fitted(model)~pop_65 | CLUSTER, groups=CLUSTER, data=dat, type=c('p','r'), auto.key=F)


ggplot(dat,aes(x=school_closing))+geom_bar()+facet_grid(~CLUSTER)+theme_bw() 
ggplot(dat,aes(x=stay_home_restrictions))+geom_bar()+facet_grid(~CLUSTER)+theme_bw() 
ggplot(dat,aes(x=gatherings_restrictions))+geom_bar()+facet_grid(~CLUSTER)+theme_bw() 



