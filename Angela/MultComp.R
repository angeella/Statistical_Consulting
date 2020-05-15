#New analysis

#load package
source("Angela/packages.R")

#load variables
load("Angela/Data/var.RData")

#load data
source("Angela/loadData.R")

#Manage date
dat$dateF <- factor(dat$date)
dat$Clusters <- factor(dat$Clusters)
levels(dat$Clusters) <- c("Restrictions", "Tracing", "Testing")

#graph
dat %>% ggplot(aes(x=date, y=R0mean, group=Clusters, color=Clusters)) + 
  geom_point(size=1) + theme_classic()

dat %>% ggplot(aes(x=date, y=R0mean, group=Clusters, color=Clusters)) + 
  geom_smooth()

#Three times: 15 Febr , 1 Apr
dat$EPOCH <- ifelse(dat$date <= "2020-02-15", "First",
                    ifelse(dat$date <= "2020-04-01", "Second", "Third"))

ggplot(dat, aes(x=EPOCH, y=R0mean, fill=EPOCH)) + geom_boxplot() 



f <- as.formula(paste("R0mean", "~", paste(c(var_EC[c(2,3,4)]), collapse=" + "), 
                      "+", paste(c(var_FIX[c(1,4,5,6)]), collapse=" + "),
                      "+", paste(c(var_HS[c(5,6)]), collapse=" + "),
                      "+", "tests + Clusters",
                      "+ (1|EPOCH)"))
model1 <- lmer(f, data = dat,REML = FALSE)

summary(model1)
comparison <- c("Restrictions - Tracing = 0",
                "Restrictions - Testing = 0",
                "Tracing - Testing = 0")

test<-multcomp::glht(model1, linfct = mcp(Clusters = comparison),adjusted="holm")
summary(test)

eff <- data.frame(effect("Clusters", model1))
#eff$Group <- factor(eff$Group,levels = c("cnt","fep","scz", "bipo"))

ggplot(eff, aes(x=Clusters, y=fit)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(group = 1)) + 
  geom_point(aes(x=reorder(Clusters, upper, desc), y=fit))+
  xlab("Clusters") + ylab("R0mean")

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



