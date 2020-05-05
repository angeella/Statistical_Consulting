lagdata <- function(dataset, vars=c(), lag=1, save.lag=T, save.var=F, prefix.lag="", suffix.lag=".lag(n)", prefix.var="", suffix.var=".var(n)") {
  require(dplyr)
  idrecord <- c("id", "state", "date")
  
  if (!any(save.lag, save.var)) {
    stop("nothing to be done")
  }
  
  writeLines("casting variable 'date' to Date with as.Date(...)")
  dataset$date <- as.Date(dataset$date)
  
  if (length(err <- setdiff(vars, colnames(dataset))) > 0) {
    stop(paste("variables", paste(err, collapse = ", "), "are not in the dataset"))
  }
  prefix.lag <- prefix.lag %>% gsub(pattern = "\\(n\\)", replacement = lag)
  suffix.lag <- suffix.lag %>% gsub(pattern = "\\(n\\)", replacement = lag)
  newlag <- paste0(prefix.lag, vars, suffix.lag)
  if (any(newlag %in% colnames(dataset))) {
    stop(paste("dataset already contains variables:", paste(intersect(newlag, colnames(dataset)), collapse = ", ")))
  }
  
  lagged <- dataset[,c(idrecord, vars)]
  colnames(lagged) <- c(idrecord, newlag)
  lagged$date <- lagged$date + lag # vedi prima
  
  lagged <- dataset[,idrecord] %>% left_join(lagged, by=idrecord)
  
  if (save.var) {
    prefix.var <- prefix.var %>% gsub(pattern = "\\(n\\)", replacement = lag)
    suffix.var <- suffix.var %>% gsub(pattern = "\\(n\\)", replacement = lag)
    newvar <- paste0(prefix.var, vars, suffix.var)
    if (any(newvar %in% colnames(dataset))) {
      stop(paste("dataset already contains variables:", paste(intersect(newvar, colnames(dataset)), collapse = ", ")))
    }
    varied <- dataset[,c(idrecord, vars)]
    colnames(varied) <- c(idrecord, newvar)
    varied[,newvar] <- varied[,newvar] - lagged[,newlag]
  }
  
  if (save.lag) {
    writeLines(paste("variables added:", paste(newlag, collapse = ", ")))
    dataset <- dataset %>% left_join(lagged, by=idrecord)
  }
  if (save.var) {
    writeLines(paste("variables added:", paste(newvar, collapse = ", ")))
    dataset <- dataset %>% left_join(varied, by=idrecord)
  }
  
  return(dataset)
}