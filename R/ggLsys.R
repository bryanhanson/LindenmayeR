##' S3 method to draw a 2D L-System with ggplot2
##'
##' This function takes either returned dataframe from Lsys from \code{\link{recLsys}}
##' or initial strings to create which, to create plots with ggplot
##'
##' @param x A character vector, or dataframe containing all required info to plot Lsys. For
##' details, see \code{\link{drawLsys}}
##'
##'
##' @return a gg object, by default it would be plotted.
##'
##' @name ggLsys
##' @rdname ggLsys
##' @export
##' @keywords plot
##'



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
  #x = recLsys(string=x,drules,st,stepSize,ang,which,shrinkFactor)

  x =do.call(recLsys,list(string = x,drules=drules,st=st,stepSize=stepSize,ang=ang,
                          which = which,shrinkFactor=shrinkFactor))
  ggLsys.data.frame(x)

}
