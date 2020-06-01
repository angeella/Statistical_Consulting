
#Compute R0 and merge in the dataset.

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

#count_confirmed <- dat %>% group_by(id) %>% summarise(Frequency = sum(confirmed))

#count_confirmed$id[which(count_confirmed$Frequency==0)]

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

datMobilityITA <- datMobilty %>% filter(id == "ITA")

R0_ITA <- R0_compute(state="ITA", datMobilityITA)
colnames(R0_ITA)[1] <- "date"

datITA <- full_join(R0_ITA, datMobilityITA, by = "date")

plot(datITA$mean.ITA, datITA$workplaces_percent_change_from_baseline)

