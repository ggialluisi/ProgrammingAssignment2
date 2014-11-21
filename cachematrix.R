##
## The idea is to be able to get a cached version of the inverse of a matrix.
##
## Given your matrix 'mySqareInversibleMatrix' (note it must be a square and 
## inversible matrix!), with the command:
##
## > myCacheMatrix <- makeCacheMatrix(mySqareInversibleMatrix)
##
## you will create an object myCacheMatrix that can be passed as parameter
## to the cacheSolve() function. This functioin will return the inverse matrix,
## but it will check if it is already cached.
##
## So, running following commands:
##
## > myCacheMatrix <- makeCacheMatrix(mySqareInversibleMatrix)
## > cacheSolve(myCacheMatrix)
## > cacheSolve(myCacheMatrix)
##
## you will see the 'getting cached data' message after the second cacheSolve calling,
## indicating that the solve function was not executed again.
##



## function makeCacheMatrix
##
## Creates a special king of 'matrix', one that implements the following functions:
## set, get, getsolve and setsolve. They are needed for caching the inverse of the matrix x.
## The matrix 'x' and the its inverse 's' are stored in the parent environment.
makeCacheMatrix <- function(x = matrix()) {
    ## defined property to store the cached version
    s <- NULL
    
    ## defined functions inside the scope of the return object
    set <- function(y) {
        x <<- y ## changes the x in the parent environment, 
                ## that is the function makeCacheMatrix
        s <<- NULL
    }
    get <- function() {
        x
    }
    setsolve <- function(solve) {
        s <<- solve 
    }
    getsolve <- function() {
        s
    }
    
    ## returns a list that can run functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}




## The function cacheSolve below returns the inverse matrix of the x matrix
## of an object created using makeCacheMatrix() function.
## Checks if cached version is avaliable, and caches it if necessary.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cachedSolve <- x$getsolve()
    if(!is.null(cachedSolve)) {
        message("getting cached data")
        return(cachedSolve)
    }
    
    ## was not cached, let's calculate and cache
    data <- x$get()
    cachedSolve <- solve(data, ...)
    x$setsolve(cachedSolve)
    
    ## always return cached data
    cachedSolve
}



## dear corrector peer, thank you for getting so far!
## most comments on the code above should have a '?' mark in the end...
## please correct me if I said something wrong on them.
## thank you