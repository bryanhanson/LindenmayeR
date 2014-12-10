##' 3D Rotation Matrices for use with Lindenmayer Systems
##'
##' These functions generate rotation matrices for use with Lindenmayer or L-Systems.
##' Called by \code{\link{drawLsys3D}} and not normally by the user.
##'
##' @param a Numeric.  An angle in radians.
##'
##' @return A 3 x 3 Matrix.
##'
##' @section Details These matrices correspond to a right-hand system, with (+)-ive
##' rotation being clockwise from the reference axis.  See
##' the coordinate system diagram in the README.  These matrices should
##' premultiply the coordinates to get the new coordinates.
##'
##' @name Rh-Rl-Ru
##' @rdname Rh-Rl-Ru
##' @alias Ru
##' @alias Rl
##' @alias Rh
##' @export
##' @keywords utilities
##'

Rh <- function(a) { # rotation around x/H axis
	M <- c(1.0, 0.0, 0.0, 0.0, cos(a), sin(a), 0.0, -sin(a), cos(a))
	M <- matrix(M, ncol = 3, byrow = TRUE)
	M
	}
	
Rl <- function(a) { # rotation around y/L axis
	M <- c(cos(a), 0.0, sin(a), 0.0, 1.0, 0.0, -sin(a), 0.0, cos(a))
	M <- matrix(M, ncol = 3, byrow = TRUE)
	M
	}
	
Ru <- function(a) { # rotation around z/U axis
	M <- c(cos(a), -sin(a), 0.0, sin(a), cos(a), 0.0, 0.0, 0.0, 1.0)
	M <- matrix(M, ncol = 3, byrow = TRUE)
	M
	}

