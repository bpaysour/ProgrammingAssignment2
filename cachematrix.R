## The following functions compute the inverse of a matrix and cache the value
## so that if the inverse is needed again, it can simply be looked up in the
## cache rather than recomputed. 

## The function 'makeCacheMatrix' creates a special "matrix" object that is a list 
## containing functions to
##
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get, setmean = setmean, getmean = getmean)
}


## The function 'cacheSolve' calculates the inverse of the special "matrix" object
## created by 'makeCacheMatrix'.  It first checks whether the inverse already has
## been calculated.  If so, it gets the inverse from the cache and skips the 
## computation.  Otherwise it calculates the inverse of the matrix and sets the
## value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
