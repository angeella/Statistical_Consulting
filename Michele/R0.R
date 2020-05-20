
require(COVID19)
require(Smisc)
require(fda)

require(EpiEstim)

dat <- covid19(level = 1, country=c("Italy")) %>%
  dplyr::rename(country=administrative_area_level_1, region=administrative_area_level_2) %>%
  dplyr::arrange(id, date) %>%
  dplyr::mutate(active=confirmed-deaths-recovered)

estimate_R(dat$active)

# smooth.monotone(dat$Rmod, dat$Smod)

plot(dat$R0, log="y")
abline(h=1)
dplyr::lag(dat$Rmod, )
lines(x=dat$Rmod, y=splinefun(x=dat$Rmod, y=dat$Smod, method = "hyman")(dat$Rmod))
splinefun(x=dat$Smod, y=dat$Rmod, method = "hyman")(dat$Smod)

dat %>%
  ggplot() +
  geom_line(aes(x=date, y=R0, group=id, color=region)) +
  geom_abline(intercept = 1, slope = 0)

dat %>%
  ggplot() +
  geom_line(aes(x=date, y=1000*epicurve.smooth/population, group=id, color=id))

genWeib <- generation.time("weibull", with(list(K=2.826, L=5.665), c(mean=L*gamma(1+1/K), sd=L*sqrt(gamma(1+2/K) - gamma(1+1/K)^2))))
genLogN <- generation.time("lognormal", with(list(M=1.644, S2=0.363^2), c(mean=exp(M+0.5*S2), sd=sqrt((exp(S2)-1)*exp(2*M+S2)))))

estR0 <- estimate.R(
  (dat %>%
    filter(id=="ITA") %>%
    as.data.frame() %>%
    dplyr::select(epicurve) %>%
    unlist())[-c(1:50)],
  genWeib,
  methods=c("EG", "ML", "TD", "AR", "SB"), 
  pop.size=60421760,
  nsim=10000
)
str(estR0$estimates$TD)
dweibull()
dat$population[dat$id=="ITA"]



library(R0)

## Outbreak during 1918 influenza pandemic in Germany)
data(Germany.1918)
mGT<-generation.time("gamma", c(3, 1.5))
estR0<-estimate.R(Germany.1918, mGT, begin=1, end=27, methods=c("TD", "SB"), 
                  pop.size=100000, nsim=10000)

estR0$estimates$SB$R
## $names
## [1] "epid"      "GT"        "begin"     "end"       "estimates"
## 
## $class
## [1] "R0.sR"

