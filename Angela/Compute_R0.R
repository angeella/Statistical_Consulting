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

colnames(dat)

dat_IT <- dat %>% filter(id =="ITA") 
dat_IT <- dat_IT[which(dat_IT$confirmed!=0),]
days <- dim(dat_IT)[1]
dat_IT$t <- c(1:days)

#calculate r0 based with a mobile window of 5 days
#vector for beta and standard deviation
duration<-18
beta1<-NULL
sd_beta1<-NULL
#for cycle for R0 estimates from days-2 to days+2
for (i in 3:(days-2)){
  fit <- lm(log(confirmed)~t,data=dat_IT[(i-2):(i+2),])
  beta1<-c(beta1,coef(fit)[2])
  sd_beta1<-c(sd_beta1,coef(summary(fit))[2,2])
}



mean  <- 1+(beta1*duration)
lower <- 1+((beta1-1.96*sd_beta1)*duration)
upper <- 1+((beta1+1.96*sd_beta1)*duration)
label<-as.Date(substr(dat_IT$date,1,10))[3:(days-2)]

df <- data.frame(label, mean, lower, upper)


ggplot(data=df, aes(x=label, y=mean, ymin=lower, ymax=upper)) +
geom_pointrange() +
geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
xlab("Date") + ylab("R0 Mean (95% CI)") +
theme_bw() 

#ok for each country?

R0_compute <- function(state, dataset){
  
  #dat_c <- dataset %>% filter(id == paste0(state)) 
  dat_c <- dataset[which(dataset$id== paste0(state)),]
  dat_c <- dat_c[which(dat_c$confirmed!=0),]
  days <- dim(dat_c)[1]
  dat_c$t <- c(1:days)
  
  #calculate r0 based with a mobile window of 5 days
  #vector for beta and standard deviation
  duration<-18
  beta1<-NULL
  sd_beta1<-NULL
  #for cycle for R0 estimates from days-2 to days+2
  for (i in 3:(days-2)){
    fit <- lm(log(confirmed)~t,data=dat_c[(i-2):(i+2),])
    beta1<-c(beta1,coef(fit)[2])
    sd_beta1<-c(sd_beta1,coef(summary(fit))[2,2])
  }
  
  
  
  mean  <- 1+(beta1*duration)
  lower <- 1+((beta1-1.96*sd_beta1)*duration)
  upper <- 1+((beta1+1.96*sd_beta1)*duration)
  label<-as.Date(substr(dat_c$date,1,10))[3:(days-2)]
  
  
  df <- data.frame(label, mean, lower, upper)
  colnames(df) <- c("label", paste0("mean.", state),paste0("lower.", state),paste0("upper.", state))
  return(df)
}

id_states <- unique(dat$id)

count_confirmed <- dat %>% group_by(id) %>% summarise(Frequency = sum(confirmed))

count_confirmed$id[which(count_confirmed$Frequency==0)]

dat1 <- dat %>% filter(id != "ASM")

id_states <- unique(dat1$id)

out <- sapply(id_states, function(x) R0_compute(state = paste0(x), dataset= dat1),simplify = FALSE)

#full dataset R0
df_merged <- plyr::join_all(out, by = 'label')

#la mia classificazione preliminare sarebbe: 
#HARD lockdown (Italy, Spain, France, UK, China), 
#SOFT lockdown (Ned, Belgium, USA, Turkey**, Iran**), 
#Nothing (Brazil, Sweden), 
#politiche di contenimento molto particolari: Singapore, S. Korea

#HARD LOCKDOWN
p <-ggplot(data=df_merged, aes(x=label, y=mean.ITA, ymin=lower.ITA, ymax=upper.ITA)) +
  geom_pointrange() +
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  xlab("Date") + ylab("R0 Mean (95% CI)") +
  theme_bw() + geom_pointrange(data=df_merged, aes(x=label, y=mean.ESP, ymin=lower.ESP, ymax=upper.ESP), col = "red") +
 geom_pointrange(data=df_merged, aes(x=label, y=mean.FRA, ymin=lower.FRA, ymax=upper.FRA), col = "green") +
 geom_pointrange(data=df_merged, aes(x=label, y=mean.GBR, ymin=lower.GBR, ymax=upper.GBR), col = "pink")
  
#SOFT LOCKDOWN
p1 <-ggplot(data=df_merged, aes(x=label, y=mean.NLD, ymin=lower.NLD, ymax=upper.NLD)) +
  geom_pointrange() +
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  xlab("Date") + ylab("R0 Mean (95% CI)") +
  theme_bw() + geom_pointrange(data=df_merged, aes(x=label, y=mean.BEL, ymin=lower.BEL, ymax=upper.BEL), col = "red") +
  geom_pointrange(data=df_merged, aes(x=label, y=mean.USA, ymin=lower.USA, ymax=upper.USA), col = "green") 

#Nothing 
p2 <-ggplot(data=df_merged, aes(x=label, y=mean.SWE, ymin=lower.SWE, ymax=upper.SWE)) +
  geom_pointrange() +
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  xlab("Date") + ylab("R0 Mean (95% CI)") +
  theme_bw() + geom_pointrange(data=df_merged, aes(x=label, y=mean.BRA, ymin=lower.BRA, ymax=upper.BRA), col = "red") 

#Analyze R0 across countries and changing of mobility.
dat_0 <- COVID19::covid19(level=1) %>% as.data.frame()
datMobilty <- merger(dat_0)



