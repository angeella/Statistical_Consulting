#################################MIXED MODEL######################################

#We need to take into account the nested and repeated measures structures:
#Cluster -> ID -> Time
#having covariates that change between time and covariates that not.

#Load packages
library(amap)
library(ggplot2)
library(lme4)
library(multcomp)
library(lme4)
library(ggbiplot)
library(corrgram)
library(ICC)
require(COVID19)
require(ggplot2)
require(dplyr)
require(countrycode)
require(ISOcodes)
require(stringr)
require(stringi)
require(stringdist)
require(maps)
require(rnaturalearth)
require(rnaturalearthdata)
require(psych)
library(pbkrtest)
#load function
source("Michele/lib/long2wide.R") 
source("Michele/lib/merger.R")
source("Michele/lib/policies.R") 
source("Michele/lib/lagdata.R") 
source("Angela/Compute_R0.R")
source("Angela/merge_dat_R0.R")

#load data
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

dat <- add_R0(dataset = dat)

#Filter data by countries used in the clustering from Silvia
#load cluster
uno <-as.character(readRDS("Silvia/uno.rds"))
due <-as.character(readRDS("Silvia/due.rds"))
tre <-as.character(readRDS("Silvia/tre.rds"))

states_to_sel <- unique(c(uno,due,tre))
length(states_to_sel) #27

datA <- dat %>% filter(id %in% states_to_sel)
dim(datA)

datA$Clusters <- ifelse(datA$id %in% uno, 1, 
                        ifelse(datA$id %in% due, 2, 3))

#load variables set
load("Angela/Data/var.RData")