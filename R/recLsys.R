recLsys = function(string = NULL, drules = NULL,
                            st = c(5, 50, 0), stepSize = 1.0, ang = 90.0,
                            which = length(string), shrinkFactor = NULL){

  # check drules to make sure only allowed characters were given
  OK <- c("F", "f", "+", "-", "[", "]")
  test <- drules$action
  if (!all(test %in% OK)) {
    msg1 <- paste("Only the following actions are recognized:",
                  paste(OK, collapse = " "), sep = " ")
    message(msg1)
    good <- test %in% OK
    bad <- test[!good]
    msg2 <- paste("I can't use these:",
                  paste(bad, collapse = " "), sep = " ")
    stop(msg2)
  }
  for (n in 1:length(which)) {
  string <- unlist(strsplit(string[which[n]], ""))

  for (i in 1:nrow(drules)) { # translate to drawing commands
    for (j in 1:length(string)) {
      if (string[j] == drules$symbol[i]) string[j] <- drules$action[i]
    }
  }

  # execute the drawing instructions

  grid.move.to(st[1], st[2], default.units = "native")
  cp <- st # cp = current point
  ch <- st[3] # ch = current heading, 0 = East in degrees
  if (!is.null(shrinkFactor)) stepSize <- stepSize/shrinkFactor
  fifo <- vector("list") # store info to restore later
  ns <- 0L # stack counter


  xRec = cp[1,drop =T]
  yRec = cp[2,drop = T]

  typeRec = 's' # type of coord data
  # s - start
  # d - draw
  # r - reverse
  for (j in 1:length(string))	{
    #cat("Processing character", j, "\n")
    if (string[j] == "F") {
      x <- cp[1] + stepSize*cos(ch*pi/180)
      y <- cp[2] + stepSize*sin(ch*pi/180)
      #
      cp <- c(x,y)
      xRec = c(xRec,x)
      yRec = c(yRec,y)
      typeRec <- c(typeRec,"d")
    }else	if(string[j] == "f") {
      x <- cp[1] + stepSize*cos(ch*pi/180)
      y <- cp[2] + stepSize*sin(ch*pi/180)
      cp <- c(x,y)

      xRec = c(xRec,x)
      yRec = c(yRec,y)
      typeRec<-c(typeRec,"m")
    }else if(string[j] == "[") {
      #cat("Found a [ \n")
      #cat("ns is:", ns, "\n")
      ns <- ns + 1 # save the current settings
      fifo[[ns]] <- c(cp, ch)
      #print(fifo)
    }else if(string[j] == "]") {
      #cat("Found a ] \n")
      #cat("ns is:", ns, "\n")
      cp <- fifo[[ns]][1:2]
      ch <- fifo[[ns]][3]

      ns <- ns - 1

      xRec = c(xRec,cp[1])
      yRec = c(yRec,cp[2])
      typeRec = c(typeRec,'r')
      #print(fifo)
    }else	if(string[j] == "-"){
      ch = ch - ang
    }else{
      if (string[j] == "+") ch = ch + ang
    }
  }
  }
  return(data.frame(x = xRec,y = yRec,pType = typeRec))
}
