library(tidyverse)
library(tidycovid19)

merged <- download_merged_data(cached = TRUE, silent = TRUE)

merged_sel <- merged %>% filter(iso3c %in% states_to_sel) %>% filter(date <= "2020-05-05")
map_covid19(merged_sel, type = "active", per_capita = TRUE, cumulative = TRUE)
acaps <- download_acaps_npi_data(cached = TRUE, silent = TRUE)
library(gghighlight)
library(lubridate)


df=acaps %>% filter(iso3c %in% states_to_sel) %>% filter(date_implemented <= "2020-05-05 UTC") %>%
  filter(log_type == "Phase-out measure") %>%
  mutate(week = floor_date(date_implemented, "weeks", week_start = 1)) %>%
  group_by(week, category) %>%
  summarize(nli = n())


a<-ggplot(df, aes(x = week, y = nli, fill = category)) +
  geom_col(position = "stack") + theme_minimal() +
  labs(title = "Phase-Out of Interventions over Calendar Time",
       x = "Date",
       y = "Weekly Number of Phase-Out Measures")

ggplotly(a)


dat_shape$nINT <- rowSums(dat_shape[,c(10:20)]!= 0,na.rm = T)
for(i in 1:nrow(dat_shape)){
  dat_shape$category[i] <- ifelse(dat_shape[i,c(10)]!=0 , "Social distancing",
                                  ifelse(
                               dat_shape[i,c(11)]!=0 , "Social distancing",
                               ifelse(
                               dat_shape[i,c(12)]!=0 , "Social distancing",
                               ifelse(
                               dat_shape[i,c(13)]!=0 , "Social distancing",
                               ifelse(
                               dat_shape[i,c(14)]!=0 , "Social distancing",
                               ifelse(
                               dat_shape[i,c(15)]!=0 , "Social distancing", 
                               ifelse(
                               dat_shape[i,c(16)]!=0, "Lockdown",
                               ifelse(
                               dat_shape[i,c(17)]!=0, "Movement restrictions",
                               ifelse(
                               dat_shape[i,c(18)]!=0, "Movement restrictions",
                               ifelse(
                               dat_shape[i,c(19)]!=0, "Testing policy",
                              "Contact Tracing"
                               ))))))))))
}



a<-ggplot(dat_shape, aes(x = date, y = nINT)) +
  geom_col(position = "stack") + theme_minimal() +
  labs(x = "Date",
       y = "Number of Phase-Out Measures")

ggplotly(a)

p <-ggplot(dat_shape, aes(date,nINT, fill = category)) +
  geom_col(position = "stack") + theme_minimal() +
  labs(x = "Date",
       y = "Number of Policies Measures")

ggplotly(p)
