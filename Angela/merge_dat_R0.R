
#JOIN ALL
add_R0 <- function(dataset){
  source("Angela/Compute_R0.R")
  
  dat <- dataset %>% filter(id != "ASM")
  id_states <- unique(dat$id)
  db2 <- list()
  for(i in 1:length(id_states)){
    
    db <- dat %>% filter(id == id_states[i]) 
    db1 <- R0_compute(state = id_states[i], dataset = dat %>% filter(id == id_states[i]))[,c("date", "mean")]
    colnames(db1) <- c("date", "R0mean")
    db2[[i]] <-full_join(db, db1, by = "date")
  }
  
  df <- plyr::ldply(db2, data.frame)

  
  return(df)  
}

#dat1 <- add_R0(dataset = dat)
