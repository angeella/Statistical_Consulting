#Manual cluster

#No vita pubblica (inghilterra, olanda, portogallo, Germania, repubblica ceca, polonia, norvegia, finlandia,Estonia, svizzera, lituania)
#Lockdown (Italia, francia, irlanda, spagna, grecia, turchia, Romania, Serbia, Russia,ungheria,belgio, slovacchia, Romania, croazia, Macedonia)
#Chiusura scuole e/o confini (Finlandia)
#Nessuna misura (Tunisia, marocco, bielorissia)
#apertura scuole ma vita pubblica chiusa (danimarca)
#Alcuni negozi aperti ma restrizioni di uscire (Austria e Albania)

uno <-c("GBR", "NLD", "PRT", "DEU", "CZE", "POL", "NOR", "FIN", "EST", "SWZ", "LTU")
due <-c("ITA", "FRA", "IRL", "ESP", "GRC", "TUR", "BEL", "HRV")
tre <-c("FIN")
quattro <-c("MAR", "BLR", "TUN")
cinque <- "DNK"
sei <-c("AUT", "ALB")
sette <- "USA"


states_to_sel <- unique(c(uno,due,tre, quattro, cinque, sei, sette))
length(states_to_sel) #26

datA <- dat %>% filter(id %in% states_to_sel)
dim(datA)

datAA$Clusters1 <- ifelse(datAA$id %in% uno, 1, 
                        ifelse(datAA$id %in% due, 2, 
                               ifelse(datAA$id %in% tre, 3,
                                      ifelse(datAA$id %in% quattro, 4,
                                             ifelse(datAA$id %in% cinque, 5,
                                                    ifelse(datAA$id %in% sei, 6,7))))))


datA <- add_R0_ML(datA)
datA$Clusters <- as.factor(datA$Clusters)

datAA <- datA %>% filter(date >= as.Date("2020-15-02", format= "%Y-%d-%m"))

datAA %>% ggplot(aes(x=date, y=school_closing, group=Clusters, color=Clusters)) + 
  geom_smooth()


datA %>% ggplot(aes(x=date, y=school_closing, group=id, color=id)) + 
  geom_smooth()

pal <- colors(10)[1:10]

datA %>% ggplot(aes(x=date, y=school_closing)) + 
  geom_smooth() + geom_smooth(aes(x=date, y=workplace_closing, col = pal[1])) +
  geom_smooth(aes(x=date, y=cancel_events, col = pal[2])) +
  geom_smooth(aes(x=date, y=gatherings_restrictions, col = pal[3])) +
  geom_smooth(aes(x=date, y=transport_closing, col = pal[4])) +
  geom_smooth(aes(x=date, y=stay_home_restrictions, col = pal[5])) +
  geom_smooth(aes(x=date, y=internal_movement_restrictions, col = pal[6])) +
  geom_smooth(aes(x=date, y=international_movement_restrictions, col = pal[7])) +
  geom_smooth(aes(x=date, y=information_campaigns, col = pal[8])) +
  geom_smooth(aes(x=date, y=testing_policy, col = pal[9])) +
  geom_smooth(aes(x=date, y=contact_tracing, col = pal[10]))

#Sembra 1 Marzo poi 1 Aprile
require(psych)
datAA <- datA %>% filter(date >= as.Date("2020-15-02", format= "%Y-%d-%m") & 
                         date <= as.Date("2020-01-03", format= "%Y-%d-%m"))

policies<- colnames(datA)[11:21]
W <- polychoric(datAA[,policies])
W$rho
p3 <- principal(r = W$rho,nfactors = 6) 
p3$loadings #prime 3 componenti
cbind(policies, p3$loadings)
scores <- as.matrix(datAA[,policies]) %*% p



