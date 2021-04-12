## This set of functions makes it possible to cache the inverse of an matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv_matrix <- NULL
      set <- function(y) {
 		x <<- y
		invm <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv_matrix <<- inverse
	getInverse <- function() inv_matrix
	list(set = set, 
		get = get, 
		set_inverse = setInverse, 
		get_inverse = getInverse)
}


## This function computes the inverse of the special matrix returned by 
## makeCacheMatrix above. 
## If inverse matrix is already calculated use it otherwise compute.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv_matrix <- x$getInverse()
	if(!is.null(inv_matrix)) {
		message("getting cached data")
		return(inv_matrix)
	}
	data <- x$get()
	# compute inverse
	inv_matric <- solve(data, ...)
	x$setInverse(inv_matrix)
	inv_matrix
}
