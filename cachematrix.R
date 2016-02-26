#This pair of functions is used to cache the inverse of a square matrix.

#The makeCacheMatrix function creates a special matrix object that can cache
#its inverse. It will return a list of functions to be used with the
#cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        #The following functions will:
        # set the value of the matrix
        # get the value of the matrix
        # set the inverse value of the matrix
        # get the inverse value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inver) inv <<- inver
        getinv <- function() inv
        
        #Return a list of functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

#The cacheSolve function computes the inverse of the special matrix returned
#by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        
        #Retrieve inverse value of the matrix and test if it is NULL
        #If not NULL print the cached inverse matrix
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        
        #If cached value is NULL calculate the inverse of the matrix,
        #cache the value, and print the inverse value
        imat <- x$get()
        inv <- solve(imat)
        x$setinv(inv)
        inv
}
