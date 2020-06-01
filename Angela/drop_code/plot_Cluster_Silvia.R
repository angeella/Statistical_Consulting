
datAAA <- datAA %>% filter(date >= as.Date("2020-04-01") & date <= as.Date("2020-05-13"))


datAA1 <- datAA %>% filter(id %in% uno)
datAA2 <- datAA %>% filter(id %in% due)
datAA3 <- datAA %>% filter(id %in% tre)

ggplot(datAA3, aes(x=id, y=R0_lag, fill=id)) + geom_boxplot() 

dat %>% ggplot(aes(x=date, y=school_closing, group=Clusters, color=Clusters)) + 
  geom_smooth()

datAA3 %>% ggplot(aes(x=date, y=testing_policy, group=id, color=id)) + 
  geom_smooth()

datAA1 %>% ggplot(aes(x=date, y=testing_policy, group=id, color=id)) + 
  geom_smooth()

datAA2 %>% ggplot(aes(x=date, y=testing_policy, group=id, color=id)) + 
  geom_smooth()

datAA2 %>% ggplot(aes(x=date, y=contact_tracing, group=id, color=id)) + 
  geom_smooth()


