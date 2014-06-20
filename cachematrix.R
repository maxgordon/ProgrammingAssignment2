## These functions create a vector of 4 functions to manage calculating and
## caching a matrix inverse, and either solving or retrieving the inverse
## from cache

## makeCacheMatrix creates a special "vector",
## which is really a list containing 4 functions:
## 1.set the value of the vector
## 2.get the value of the vector
## 3.setinverse to calculate the inverse of a matrix
## 4.getinverse to retrieve the inverse 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
      set <- function(y) {
		x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cachSolve calculates the inverse of a matrix, or uses the cached vesion if
## it already exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}

