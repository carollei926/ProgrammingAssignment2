## Put comments here that give an overall description of what your
## functions do

### makeCacheMatrix: first creates a special matrix then cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
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


### cacheSolve: 
###   (1) computes the inverse of the special matrix returned by makeChcheMatrix.
###   (2) if the inverse has been calculated then retrive value from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        else{
                data <- x$get()
                m <- solve(data, ...)
                x$setinverse(m)
                m
        }
}
