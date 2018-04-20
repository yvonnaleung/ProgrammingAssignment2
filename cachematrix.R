## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        #if the inverse has already been calculated
        if(!is.null(inv)) {
                #get it from the cache
                message("getting cached data")
                return(inv)
        }
        #if the inverse has not been calculated, calculate the inverse
        data <- x$get()
        inv <- solve(data, ...)
        #set the value of the inverse in the cache via the setinv function
        x$setInverse(inv)
        inv
}
