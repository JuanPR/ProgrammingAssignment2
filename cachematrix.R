## Put comments here that give an overall description of what your
## functions do

## This function creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

		m <- NULL
		set <- function(y) {
    
			x <<- y
			m <<- NULL
		}
      
		get <- function() x
        setCacheMatrix <- function(solve) m <<- solve
        getCacheMatrix <- function() m
        
		list(set = set, get = get,
                 setCacheMatrix = setCacheMatrix,
                 getCacheMatrix = getCacheMatrix)	
}


## This function computes the inverse of the "matrix" returned by makeCacheMatrix 
## If the inverse has already been computed and the matrix has not changed, 
## the cachesolve retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

		m <- x$getCacheMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        myMatrix <- x$get()
        m <- solve(myMatrix, ...)
        x$setCacheMatrix(m)
        m
		
}


 