#countries are at different stages of the pandemic. 
#Some that were effective at initial containment, such as Singapore and Hong Kong, have seen resurgence and are implementing additional measures to address it. 
#Others, such as many countries in Western Europe, have seen the number of new cases plateau or begin to decline and are debating the right approach to reopening their economies. 
#Some countries appear to be at the peak of infection and are urgently building surge capacity in their health systems. 
#In other parts of the world, the number of cases is rising rapidly. Countries such as Russia and Turkey are seeing a recent acceleration. 
#India too has experienced a significant increase in the number of cases since the beginning of April and has evolved its response strategy, including extending the nationwide lockdown.

#Local use of these measures varies considerably-physical distancing may 
#be near-impossible in crowded urban settings, 
#for example, and the apps and digital tools for contact tracing like those 
#used in China may not be acceptable in other parts of the world.
#Another challenge is the dependencies among these measures: to take one example, 
#the timeliness and stringency of physical distancing measures substantially 
#influences how other tools should be deployed.

# a few countries, such as Sweden, are pursuing an alternative 
#"herd immunity" strategy focused on protecting the most vulnerable 
#populations while using only limited distancing measures to flatten the 
#curve for others.

library(incidence)
library(EpiEstim)
source("Angela/loadData.R") 


dataIT <- dat %>% filter(country == "Spain")
date <- dataIT$date
i<- incidence(date)
i <- incidence(dataIT$confirmed)
plot(i)

IT_incidence_peak <- find_peak(IT_incidence_object)
IT_incidence_fit <- incidence::fit(IT_incidence_object)

plot(date) %>% add_incidence_fit(IT_incidence_fit)


## Estimating R on sliding weekly windows, with a parametric serial interval

out <- estimate_R(dataIT$confirmed, method = "uncertain_si", 
                  config = make_config(list(mean_si = 7.5, std_mean_si = 2, 
                                            min_mean_si = 1, max_mean_si = 8.4, std_si = 3.4, std_std_si = 1, 
                                            min_std_si = 0.5, max_std_si = 4, n1 = 1000, n2 = 1000)))

                                            
plot_Ri <- function(estimate_R_obj) {
           p_I <- plot(estimate_R_obj, "incid", add_imported_cases = TRUE)  # plots the incidence
           p_SI <- plot(estimate_R_obj, "SI")  # plots the serial interval distribution
           p_Ri <- plot(estimate_R_obj, "R")
           return(gridExtra::grid.arrange(p_I, p_SI, p_Ri, ncol = 1))
    }
                                                                                                                                    min_std_si = 0.5, max_std_si = 4, n1 = 1000, n2 = 1000)))
plot_Ri(out)







