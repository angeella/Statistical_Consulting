#Analysis reproduction number

#REF EpiEstim https://github.com/jstockwin/EpiEstimApp/wiki/Example:-EpiEstim-R-package
#REF FIGA= https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r/
#REF earlyR = https://www.repidemicsconsortium.org/earlyR/

#calculates a maximum-likelihood estimate for the reproduction number

datIT <- dat %>% filter(id == "ITA")
onset <- datIT$date
today <- as.Date("2020-05-13")
#compute the daily incidence
i <- incidence::incidence(onset, last_date = today)

plot(i, border = "white")

#From https://reader.elsevier.com/reader/sd/pii/S1201971220301193?token=13649F14DDE5D4EA34B7E573BC1CE70B6E59FDC26A3659FBD3481B1D0BB647561BB7075EC1C8D16541C8740CAE950940
mu <- 4.7 # mean in days days
sigma <- 2.9 # standard deviation in days

#https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r/
#use mean of 5.0 days and a standard deviation of 3.4
mu <- 5 # mean in days days
sigma <- 3.4 # standard deviation in days

res <- get_R(i, si_mean = mu, si_sd = sigma)
plot(res)
R_val <- sample_R(res, 1000)

plot(res, "lambdas", scale = length(onset) + 1)
abline(v = onset, lwd = 3, col = "grey")
abline(v = today, col = "blue", lty = 2, lwd = 2)
points(onset, seq_along(onset), pch = 20, cex = 3)

date_sort <- sort(datIT$date,decreasing = FALSE)

t_end <- as.integer(c(seq(4,length(datIT$date),by = 4)))
t_start <- c(2,t_end[1:(length(t_end) -1)])

res <- estimate_R(i, method = "parametric_si",
                  config = make_config(list(
                    mean_si = mu, std_si = sigma, t_start= t_start,t_end=t_end)))
plot(res)


res_uncertain_si <- estimate_R(datIT$confirmed, method = "uncertain_si", 
                               config = make_config(list(mean_si = 7.5, std_mean_si = 2, 
                               min_mean_si = 1, max_mean_si = 8.4, std_si = 3.4, std_std_si = 1, 
                               min_std_si = 0.5, max_std_si = 4, n1 = 1000, n2 = 1000)))


# calculates a relative measure of the current "force of infection" or infectivity of an outbreak
EpiEstim::overall_infectivity()


#Library R0

library(R0)

## Outbreak during 1918 influenza pandemic in Germany)
data(Germany.1918)

dataIT <- datIT$confirmed
attr(dataIT,"names") <- as.character(datIT$date)

pop.size = unique(datIT$population)

mu <- 4.7 # mean in days days
sd <- 2.9 # standard deviation in days
mGT<-generation.time("gamma", c(mu, sd))

#With this method we have:
#Exponential Growth  method= EG 
#Maximum Likelihood  method = ML
#Attack Rate  method = AR -> SIR model
#Time-Dependent  method = TD
#Sequential Bayesian  method = SB

estR0<-estimate.R(dataIT[10:length(dataIT)], 
                  mGT,  
                  methods=c("EG", "ML", "TD", "AR", "SB"), 
                  pop.size=pop.size, nsim=1000)

length(datIT$R0mean)
length(estR0$estimates$TD$R)
dataITR0mean <- datIT$R0mean
attr(dataITR0mean,"names") <- as.character(datIT$date)
plot(dataITR0mean[12:111], type = "l")
lines(estR0$estimates$TD$R[3:102], col = "red")


datAA <- add_R0_ML(dataset = dat)

datAA$Clusters <- as.factor(dat$Clusters)
ggplot(data=datAA, aes(x=date, y=R0,col = Clusters)) +geom_smooth()

ggplot(data=datAA, aes(x=date, y=R0mean,col = Clusters)) +geom_smooth()

#A me piace questo metodo per calcolare l R0 ma ditemi voi :)

#Provo a plottare un po' di nazioni cosí 

datAA_some <- datAA %>% filter(id %in% c("ITA", "ESP", "USA", "FRA"))

ggplot(data=datAA_some, aes(x=date, y=R0,col = id)) +geom_line()
ggplot(data=datAA_some, aes(x=date, y=R0mean,col = id)) +geom_line()

#devo dire che sono molto simili! a parte l asse x ma come andamento uguali

#Provo per finire il SEIR model e ciao ciao
datIT <- dat %>% filter(id == "ITA")
dataIT <- datIT[which(datIT$confirmed!=0),]

dataR <- dataIT$confirmed
attr(dataR,"names") <- as.character(dataIT$date)
days <- dim(dataIT)[1] - 1

df <- data.frame(NA, ncol = 3, nrow = length(3:(days-2)))

for (i in 3:(days-2)){
  
  estR0<-estimate.R(dataR[(i-2):(i+2)], 
                    mGT,  
                    methods=c("AR"), 
                    pop.size=pop.size, nsim=1000)
  
  df[i,] <- data.frame(R = estR0$estimates$AR$R, 
                   lower = estR0$estimates$AR$conf.int[1],
                   upper = estR0$estimates$AR$conf.int[2])
  
}

#mmmm qualcosa non va. Da riconsiderare, comunque il modello di paolin é ok come seir sir, sono molto simili.


#Provo a fare lo stesso modello ma con il nuovo R0.







