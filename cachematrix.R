## Put comments here that give an overall description of what your
## functions do

### makeCacheMatrix: first creates a special matrix then cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # initialize the inverve 
        m <- NULL
        
        # set matrix
        set <- function(matrix){
              x <<- matrix
              m <<- NULL
        }
        
        #get matrix
        get <- function() {
              x
        }
        
        # set inverse of matrix
        setinverse <- function(solve) {
              m <<- solve
        }
        
        # get inverse of matrix
        getinverse <- function() {
              m
        }
        
        # return a list of methods
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
        
        # if there's cached inverse, return from cache
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        # if no cached inverse, calculate and return result
        else{
                data <- x$get()
                m <- solve(data, ...)
                x$setinverse(m)
                m
        }
}
