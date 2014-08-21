## The 2 functions below are used to create and to work with a special object
## that stores 2 objects within: the matrix and the inverse to it;
## such an object can be used to save time while calculating inverses
## of humongous matrices by caching the inverse.


## This function provides an Object to contain the matrix and its inverse, and the tools
## to communicate with it. The reason to create a special object to store our matrix and
## the cache is to make sure that our variables are located outside the base environment
## of the user, so that he can not unintentionally change them.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                        ##inv is the cache, in which inverse matrix is stored.
                                           ##class(inv)=matrix
        set <- function(y) {               ##writes down a new matrix, and cleares the inverse
                x <<- y
                inv <<- NULL               ##when the matrix is renewed, the inverse should be recomputed
        }
        get <- function() x                ##returns the matrix currently under disposal
        setinv <- function(inverse){
                inv <<- inverse            ##overwrites the inverse matrix cache
        }
        getinv <- function() inv           ##returns the inverse matrix currently in cache
        list(set = set, 
             get = get,
             setinv=setinv,
             getinv=getinv)

}


## This function is used to calculate the inverse of a matrix, contained within the object
## created by the makeCacheMatrix function above, and to write it down inside that object.

cacheSolve <- function(x, ...) {
        ##x is the object created by makeCacheMatrix, for example: x<-makeCacheMatrix()
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
##Tip: you can use %*% operator (true matrix multiplication) to check if the inverse matrix is actually inverse!