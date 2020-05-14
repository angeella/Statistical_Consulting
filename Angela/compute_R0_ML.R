#Compute R0 by likelihood method

#FROM est.R0.TD: Wallinga, J., and P. Teunis. "Different Epidemic Curves for Severe Acute Respiratory Syndrome Reveal Similar Impacts of Control Measures." American Journal of Epidemiology 160, no. 6 (2004): 509.
#From https://reader.elsevier.com/reader/sd/pii/S1201971220301193?token=13649F14DDE5D4EA34B7E573BC1CE70B6E59FDC26A3659FBD3481B1D0BB647561BB7075EC1C8D16541C8740CAE950940
#mu <- 4.7 # mean in days days
#sigma <- 2.9 # standard deviation in days
R0_compute_ML <- function(state, dataset, mu = 4.7, sd = 2.9){
  
  require(R0)
  dat_c <- dataset[which(dataset$id== paste0(state)),]
  dat_c <- dat_c[which(dat_c$confirmed!=0),]
  mGT<-generation.time("gamma", c(mu, sd))
  datA <- dat_c$confirmed
  attr(datA,"names") <- as.character(dat_c$date)
  estR0<-R0::estimate.R(datA, 
                    mGT,  
                    methods= "TD",
                    nsim=1000)
  R0 <- estR0$estimates$TD$R
  date <- names(R0)
  df <- data.frame(date = date, R0 = R0)
  rownames(df) = NULL
  return(df)
}

add_R0_ML <- function(dataset){

  dat <- dataset %>% filter(id != "ASM")
  id_states <- unique(dat$id)
  db2 <- list()
  for(i in 1:length(id_states)){
    
    db <- dat %>% filter(id == id_states[i]) 
    db1 <- R0_compute_ML(state = id_states[i], dataset = dat %>% filter(id == id_states[i]))
    
    db1$date <- as.Date(db1$date)
    db2[[i]] <-full_join(db, db1, by = "date")
  }
  
  df <- plyr::ldply(db2, data.frame)
  
  
  return(df)  
}