
#Compute R0 and merge in the dataset.

dat <- COVID19::covid19(level=1) %>% as.data.frame()
colnames(dat)[colnames(dat)=="iso_alpha_3"] <- "country"
cumul <- c("deaths", "confirmed", "tests", "recovered")
instant <- c("hosp", "icu", "vent")
demo <- colnames(dat)[grepl("^pop", colnames(dat))]
geo <- c("country", "state", "city", "lat", "lng")
index <- "stringency_index"
id <- "id"
time <- "date"

#dat <- dat %>% lagdata(vars=c(cumul, policies, index), lag = 1, save.lag = F, save.var = T)

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


dat <- datA
