
# id e time devono essere le prime due colonne
reshapami=function(dataset){
  d=dim(dataset)[2]
  datashape=list()
  for (i in 3:d){
    datashape[[i-2]]=spread(data = dataset[,c(1,2,i)],
              key = date,
              value = names(dataset)[i])
  }
  names(datashape)=names(dataset)[-c(1,2)]
  return(datashape)
}

