long2wide <- function(dataframe, id, time, vars) {
  res <- lapply(vars, function(v) {
    require(reshape2)
    f <- as.formula(paste(paste(id, collapse = "+"), "~", paste(time, collapse = "+")))
    aux <- dcast(dataframe, f, value.var=v) # questa funzione facile da usare con una var per volta, donde l' lapply
    aux <- as.data.frame(aux) # perché dcast crea un datatable. è molto simile, ma così evitiamo etc
    colnames(aux) <- paste0(v, colnames(aux)) # rende le variabili uniche prima del merge fatto da do.call
    if (is.factor(dataframe[,v]) | is.ordered(dataframe[,v])) {
      for (w in setdiff(colnames(aux), paste0(v, id))) { # escluse le variabili identificative
        aux[,w] <- ordered(as.character(aux[,w]), levels=names(policies.levels[[v]])) # perché alcuni livelli potrebbero essere andati persi
      }
    }
    aux
  })
  res <- do.call("cbind", res) # da una lista di dataset fa il merge delle colonne
  res <- res[,setdiff(colnames(res), apply(expand.grid(vars, id), 1, paste, collapse="")[-c(1:length(id))])] # conserva una sola delle repliche delle variabili identificative
  colnames(res)[1:length(id)] <- id
  res
}