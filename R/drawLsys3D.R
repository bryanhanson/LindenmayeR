##' Draw a 3D L-System Using Turtle Graphics
##'
##' This function takes input strings, previously created with \code{\link{Lsys}},
##' translates them into 3D turtle graphics instructions, and then plots the results.
##' 
##' @param string A character vector giving the strings containing the turtle graphics
##' instructions.  Created by \code{\link{Lsys}}.
##'
##' @param drules A data frame containing columns "symbols" and "action".  These contain the input
##' symbols and the corresponding drawing action.  See the examples.
##'
##' @param st A numeric vector of length 3 giving the screen coordinates where
##' the start of the curve should be placed.  The screen is 100 x 100 with the
##' lower left corner as 0,0.  The third element is the initial drawing angle
##' in degrees.
##'
##' @param stepSize Numeric.  The length of the drawing step.
##'
##' @param ang Numeric.  The angle in degrees when a change in direction is requested.
##'
##' @param which Integer.  The entries in \code{string} which should be drawn.  Defaults
##' to the last (most complex) entry.  If \code{length(which) > 1} each plot is drawn in
##' its own window.
##'
##' @param shrinkFactor A numeric vector of the same length as \code{string}.  As each
##' plot is made, \code{stepSize} will be divided by the corresponding value in \code{shrinkFactor}.
##' This allows one to scale down the increasingly large/complex plots to make them
##' occupy a space similar to the less complex plots.
##'
##' @param ...  Additional parameters to be passed to the \code{grid} drawing routines.
##' Most likely, something of the form \code{gp = gpar(...)}.  See \code{\link{gpar}}
##' and the last example.
##'
##' @section Warning: Remember that if \code{retAll = TRUE}, \code{\link{Lsys}} returns the initial string plus the results
##' of all iterations.  In this case, if you want the 5th iteration, you should specify \code{which = 6} since
##' the initial string is in \code{string[1]}.
##' 
##' @return None; side effect is a plot.
##' 
##' @name drawLsys3D
##' @rdname drawLsys3D
##' @export
##' @keywords plot
##'


drawLsys3D <- function(string = NULL, drules = NULL,
	stepSize = 1.0, angle = 45,
	which = length(string), ...) {

	# check drules to make sure only allowed characters were given
	OK <- c(LETTERS, letters, "+", "-", "[", "]", "^", "&", "\\", "/", "|")
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

		# convert the initial string into drawing instructions
		# F = move forward; f = move w/o drawing
		# [ save cp, ch; ] restore saved cp, ch
		# ^ = pitch up, & = pitch down (Rl)
		# \ = roll left, / = roll right (Rh)
		# + = turn left, - = turn right (Ru)
		# input angles are in degrees, functions need radians
		
		sring <- unlist(strsplit(string[which[n]], ""))	

		for (i in 1:nrow(drules)) { # translate to drawing commands
			for (j in 1:length(sring)) {
					if (sring[j] == drules$symbol[i]) sring[j] <- drules$action[i]
					}		
			}

		fifo <- vector("list") # store info to restore later
		ns <- 0L # stack counter
			
		# execute the drawing instructions
		cp <- c(0, stepSize, 0) # cp = current position
		# assumes there was a hidden step from 0,0,0 along H axis
		# the H axis is the +y axis in rgl
		# necessary b/c rotation matrices will give 0,0,0 otherwise
		
		open3d(...)

		for (j in 1:length(sring))	{
			#cat("Processing character", j, "\n")
			if (sring[j] == "F") {
				 x <- cp[1] + stepSize*cos(ch*pi/180)
				 y <- cp[2] + stepSize*sin(ch*pi/180)
				 grid.line.to(x, y, default.units = "native", ...)
				 cp <- c(x, y)
				 }

			if (sring[j] == "f") {
				 x <- cp[1] + stepSize*cos(ch*pi/180)
				 y <- cp[2] + stepSize*sin(ch*pi/180)
				 grid.move.to(x, y, default.units = "native")
				 cp <- c(x, y)
				 }

			if (sring[j] == "[") {
				#cat("Found a [ \n")
				#cat("ns is:", ns, "\n")
				ns <- ns + 1 # save the current settings
				fifo[[ns]] <- c(cp, ch)
				#print(fifo)			
				}

			if (sring[j] == "]") {
				#cat("Found a ] \n")
				#cat("ns is:", ns, "\n")
				cp <- fifo[[ns]][1:2]
				ch <- fifo[[ns]][3]
				grid.move.to(cp[1], cp[2], default.units = "native")
				ns <- ns - 1
				#print(fifo)			
				}

			if (sring[j] == "+") cp <- cp %*% Ru(angle*pi/180)

			if (sring[j] == "-") cp <- cp %*% Ru(-1*angle*pi/180)

			if (sring[j] == "&") cp <- cp %*% Rl(angle*pi/180)

			if (sring[j] == "^") cp <- cp %*% Rl(-1*angle*pi/180)

			if (sring[j] == "\\") cp <- cp %*% Rh(angle*pi/180)

			if (sring[j] == "/") cp <- cp %*% Rh(-1*angle*pi/180)

			if (sring[j] == "|") cp <- cp %*% Ru(pi) # turn around

			
			}
		} # end of looping over which
		
	}
	