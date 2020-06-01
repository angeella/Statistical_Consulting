#Clustering between epoch

#I will takes all the states

dat1 <- dat %>% filter(date >= as.Date("2020-02-15") & date <= as.Date("2020-03-15"))
dat2 <- dat %>% filter(date >= as.Date("2020-03-15") & date <= as.Date("2020-04-15"))
dat3 <- dat %>% filter(date >= as.Date("2020-04-15") & date <= as.Date("2020-05-15"))

W <- polychoric(dat1[,policies])
W$rho
p3 <- principal(r = W$rho, nfactors = 6) 
p3$loadings #prime 3 componenti
cbind(policies, p3$loadings)

W <- polychoric(dat2[,policies])
W$rho
p3 <- principal(r = W$rho, nfactors = 6) 
p3$loadings #prime 3 componenti
cbind(policies, p3$loadings)


