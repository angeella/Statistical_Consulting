


smoothing=function(data){
  ncfund=fdata(as.matrix(data[,-1]))
  optbasis=optim.basis(ncfund, #cerca il numero di basi ottime via cross-validation
                       type.CV = GCV.S,
                       lambda = 0,
                       numbasis = floor(seq(ncol(ncfund)/10, ncol(ncfund)/2, len = 10)),
                       type.basis = "bspline")$numbasis.opt
  
  p=dim(data)[2]-1
  basis <- create.bspline.basis(c(0,p), nbasis = optbasis, norder = 4)
  smoothedata<- smooth.basis(1:p, t(as.matrix(data[,-1])), basis)$fd
  par(mfrow=c(1,2))
  plot.fdata(ncfund)
  plot(smoothedata)
  return(smoothedata)
}
data=resdat$confirmed


