## Put comments here that give an overall description of what your
## functions do
# This scripts has two functions makeCacheMatrix and cacheSolve.
# Main point of these functions is to be able to retreve inverse of a matrix from cache rather
# than always calculating it, which can be slow


## Write a short comment describing this function
# Creates a cache for a matrix which can be set and retrieved (get)
# Also sets and gets the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    set <- function(y) {
        x <<- y
        invM <<- NULL
    }
    get <- function() x
    setInvMatrix <- function(InvMat) invM <<- InvMat
    getInvMatrix <- function() invM
    list(set = set, get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix
         )
}

## Write a short comment describing this function
# Finds out if the inverse of the matrix in x is solved.
# If it has been, then returns it from the cache.
# Otherwise solves the inverse of the matrix and stores it in the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invM <- x$getInvMatrix()
    if (is.null(x)){
        message("getting cached data!")
        return(invM)
    }
    data <- x$get()
    invM <- solve(data)
    x$setInvMatrix(invM)
    invM
}

