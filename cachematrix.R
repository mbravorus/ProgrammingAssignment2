## makeCacheMatrix() creates a compound object with methods to store and retrieve
## a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL

	set <- function(value) {
		x <<- value
		inv <<- NULL
	}

	get <- function() {
		x
	}

	setinverse <- function(value) {
		inv <<- value
	}

	getinverse <- function() {
		inv
	}

	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve() takes a compound object previously created by makeCacheMatrix()
## and calculates the inverse of the matrix contained. If the inverse hasn't been computed before,
## it gets stored into the compound object as a cached value and will be retrieved on subsequent invocations.
## Explicit testing whether the matrix has changed is not required due to the fact that upon setting a new 
## value for the stored matrix, cached inverse matrix is reset to NULL and will be recalculated on first hit

cacheSolve <- function(x) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()

	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}

	m <- solve(x$get())
	x$setinverse(m)

	m

}
