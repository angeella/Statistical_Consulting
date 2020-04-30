library(COVID19)
data <- covid19()

table(data$stringency_index) #Come é calcolato?
table(data$contact_tracing) #0     1     2
table(data$school_closing) # 0     1     2     3 
table(data$gatherings_restrictions) #0     1     2     3     4 
table(data$transport_closing)#0     1     2 
table(data$stay_home_restrictions) #0     1     2     3 
table(data$internal_movement_restrictions) #0  1  2
table(data$international_movement_restrictions) #0     1     2     3     4 
table(data$information_campaigns)#0     1     2 
table(data$testing_policy) #0  1  2  3

table(data$country)
#122 observations for country.

#Possibile suddivisione:

#1. Olanda, Portogallo, Norvegia (no social activity)
#2. Italy, China, France, Spain (lockdown)
#3. Finlandia (school closed and frontiers)
#4. Danimarca (school open but no social life)
#5. Austria, Albania (some open shops but restriction to go out)
#6. Tunisia, Marocco (nothing)

unique(data$contact_tracing[data$country== "Italy"]) #0 2 
unique(data$school_closing[data$country== "Italy"]) #0 3
unique(data$gatherings_restrictions[data$country== "Italy"])# 0 4  
unique(data$transport_closing[data$country== "Italy"]) #0 1 2 
unique(data$stay_home_restrictions[data$country== "Italy"]) #0 2 3
unique(data$internal_movement_restrictions[data$country== "Italy"])# 0 2  

#Abbiamo delle differenze in questo db tra Spagna e Italia come livello di lockdown,anche per la francia
max(unique(data$internal_movement_restrictions[data$country== "Spain"]))

#
unique(data$country[data$stay_home_restrictions==3 
                    & data$school_closing==3 
                    & data$gatherings_restrictions == 4
                    & data$transport_closing==2
                    & data$internal_movement_restrictions ==2
                    & data$contact_tracing == 2]) 




