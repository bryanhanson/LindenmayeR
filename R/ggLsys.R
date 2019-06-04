ggLsys=function(x,...) UseMethod("ggLsys")


ggLsys.data.frame = function(x,...){
  requireNamespace('ggplot2')
  alpha= rep(1,(nrow(x)))#color all paths with no transparency
  index_rev = which(as.vector.factor(x$pType)=="r")

  index_beforeRev = index_rev-1
  for(i in index_beforeRev) alpha[i] = .1

  x$alpha = alpha

  ggplot(data =x,aes(x = x,y = y,
                           alpha = alpha)) + geom_path()
}

ggLsys.character = function(x, drules = NULL,
                                 st = c(5, 50, 0), stepSize = 1.0, ang = 90.0,
                                 which = length(string), shrinkFactor = NULL){
  x = recLsys(string = x,drules,st,stepSize,ang,which,shrinkFactor)
  ggLsys.data.frame(x)

}
