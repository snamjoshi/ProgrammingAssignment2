## The makeCacheMatrix function takes a matrix as input which can be cached once the 
## cacheSolve function is called. The cacheSolve function takes the input matrix from 
## makeCacheMatrix, and sents it to makeCacheMatrix (get function) to be cached. If the 
## inverse is already cached it simply returns this matrix from memory.

## Takes a square (invertible) matrix as input and sets the value to the global variable x.
## Can return the cached inverse computed by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {  
		x <<- y
		m <<- NULL 
	}
	get <- function() x
	setInverse <- function(Inverse) m <<- Inverse
	getInverse <- function() m
	list(set = set, get = get,
			 setInverse = setInverse,
			 getInverse = getInverse)
}

## Takes the matrix above (x), computes its inverse using the solve() function, caches it, 
## and returns it. If the inverse is already cached (which is checked using x$getInverse()),
## it returns the cached matrix.

cacheSolve <- function(x, ...) {
	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}
