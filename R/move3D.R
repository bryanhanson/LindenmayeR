##' Translate a point in 3D
##'
##' This function translates a point in 3D, given a starting point, a current heading,
##' and a step size. Called by \code{\link{drawLsys3D}} and not normally by the user.
##'
##' @param cp A numeric vector of length 3 giving the current x, y, z coordinates of the point.
##'
##' @param ch A numeric vector of length 3 giving the current heading, described by
##' three angles H, L, U, given in degrees.  See Details.
##'
##' @param stepSize Numeric.  The length of the drawing step.
##'
##' @return A numeric vector of length 3 giving the new x, y, z coordinates for the point.
##'
##' @section Details The assumption of an L-system is that only one thing is done at a
##' time.  Thus, this function give correct answers only if two of H, L and U are zero.
##' If two values were non-zero, the answer would depend upon the order of operations.
##' This situation is even more restrictive however, since the value of H is ignored - it
##' corresponds to a barrel roll and does not affect the translation to the new position.
##' See the README for the coordinate system.
##'
##' @name move3D
##' @rdname move3D
##' @export
##' @keywords utilities
##'

move3D <- function(cp, ch, stepSize) {

	# Check that either L or U is zero (H is ignored)
	LU <- floor(abs(ch[2:3]))
	print(LU)
	if ((LU[1] > 0L) & (LU[2] > 0L)) stop("Either L or U must be zero")
	
	# Using spherical coordinates to Cartesian coordinate eqns
	# based on the idea of a unit sphere
	
	# H <- ch[1]*pi/180 # not used: corresponds to a barrel roll
	L <- ch[2]*pi/180
	U <- ch[3]*pi/180
	x <- stepSize*cos(U)*cos(L)
	y <- stepSize*sin(U)*cos(L)
	z <- stepSize*sin(L)

	#print(x^2 + y^2 + z^2) # check
	
	return(c(x, y, z) + cp) # returns updated cp
	}


# some tests
#             x, y, z          H, L, U
# move3D(cp = c(0, 0, 0), ch = c(0, 0, 0), stepSize = 1.0)
# move3D(cp = c(0, 0, 0), ch = c(90, 0, 0), stepSize = 1.0)
# move3D(cp = c(0, 0, 0), ch = c(0, 0, 90), stepSize = 1.0)
# move3D(cp = c(0, 0, 0), ch = c(0, 0, -90), stepSize = 1.0)
# move3D(cp = c(0, 0, 0), ch = c(0, 90, 0), stepSize = 1.0)
# move3D(cp = c(0, 0, 0), ch = c(0, -90, 0), stepSize = 1.0)

# move3D(cp = c(0, 0, 0), ch = c(0, 45, 0), stepSize = 1.0)
# move3D(cp = c(0, 0, 0), ch = c(0, 0, 45), stepSize = 1.0)
# move3D(cp = c(0, 0, 0), ch = c(45, 0, 0), stepSize = 1.0)

# move3D(cp = c(0, 0, 0), ch = c(45, 90, 90), stepSize = 1.0) # error trapping

