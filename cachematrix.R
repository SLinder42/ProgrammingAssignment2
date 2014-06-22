## This entire function caches a matrix, inverts it,
## and stores the inversion in the cache

## First, this function creates the special matrix object
## in the cache using a given matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # Sets inv in main environment to NULL
    set <- function(y) {
        x <<- y
        # Sets x in the local environment
        inv <<- NULL
        # Sets inv in local environment to NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inv
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    # Sets up special list which has room for matrix and inverse
    # in main and local environments separately
}


## Second, this function inverts the cached matrix but uses
## the already inverted matrix if cached

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <-x$getinv()
    # Checks for the inverse in the cache
    if(!is.null(inv)) {
    # If the inverse is in the cache
        message("getting cached data")
        return(inv)
        # Say it is in cache and then return it
    }
    else{
        data <- x$get
        # Call the matrix to be solved
        inv <- solve(data)
        # Perform calculation to get the inverse of the matrix
        # assuming matrix is a square matrix
        x$setinv(inv)
        # Set the inverse in the cache
        return(inv)
        # Return the inverse of the matrix
    }
}
