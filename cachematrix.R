## Put comments here that give an overall description of what your
## functions do

#
# Solution by Eduardo Berrocal
# ============================
# 
# The solution to this assignment is made of two functions. The first
# function creates a matrix "object" (as in object oriented programming) 
# with two state variables (the matrix 'x', and the inverse of the matrix
# 'inv_x'), and a list of functions to modify its state (in this case
# just the classic 'getters' and 'setters' of the enclosed state).
#
# The Second function calculates the inverse of the matrix object that we
# just created with the other function. It takes advantage of the object
# state in order to cache the solution of the solve() function -- which can
# be very expensive for large matrices. If the matrix returned by getinverse()
# in NULL, we need to call solve(). If it is not, we just return that matrix
# without calling solve(). 
#
# In order to make sure that the inverse variable 'inv_x' always has the 
# inverse of the matrix 'x', we reset it to NULL when the object changes 
# with the setter function set(). Furthermore, 'inv_x' is always set to
# NULL during initialization (that is, the first call to the function
# makeCacheMatrix(). 

## Write a short comment describing this function

# This function creates a new object 'CacheMatrix' which stores not only
# the matrix object 'x', but also its inversion in 'inv_x'. It has getters
# and setters functions to make sure that those state varibles are modified
# correctly (e.g., if 'x' changes, then 'inv_x' should go back to NULL).

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inv_x <<- inv
    getinverse <- function() inv_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

# This function calculates the inversion of the
# 'CacheMatrix' object created with the other function.
# This function controls the logic that makes sure that
# if the inversion of 'x' already exits, we don't calculate
# it again.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getinverse()
    if (!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    }
    data <- x$get()
    inv_x <- solve(data, ...)
    x$setinverse(inv_x)
    inv_x
}

