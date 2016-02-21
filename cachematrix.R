## The following two functions are used to cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "matrix" object
## that can cache its inverse.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function (inverse) inv <<- inverse
	getinverse <- function() inv
	list (set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by the 
## previous function. If the inverse has already been calculated (and the 
## matrix has not changed), then the cacheSolve should retrive the inverse
## from the cache.
## For this assignment, the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}
