### Functions to create a cache in which a matrix and its inverse are stored, as well as a function to pull the inverse
### out of the cache if the inverse is stored. If not, solve for the inverse and store it in the cache.

### makeCacheMatrix function ###
## create a parent environment in which a matrix and its inverse are set
## as well as contains a list of functions to set and retrieve the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) { ## matrix x is set
    inv <- NULL ## set inverse to null
    ## create set() fn
    set <- function(y) {
        x <<- y ## assign new matrix y to var x
        inv <<- NULL ## reset inverse to null
    }
    
    ## create get() fn
    get <- function() x ## get matrix
    
    ## create setinv() fn
    setinv <- function(inverse) inv <<- inverse ## set the inverse of a matrix to var inv
    
    ## create getinv() fn
    getinv <- function() inv
    
    ## create list so functions are easily called
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
    
}

### cacheSolve function ###
## check to see if the inverse of a given matrix is already stored in the cache
## if so, retrieve it from the cache
## if not, calculate the inverse, store it in the cache, and print the inverse
cacheSolve <- function(x, ...) { ## where x is a var for the environment created by makeCacheMatrix
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) { ## if the inverse is not a NULL var
        message("Retrieving inverse from cache")
        return(inv) ## this will exit out of the function
    }
    mat <- x$get() ## pull matrix from the cache
    inv <- solve(mat) ## solve for the inverse
    x$setinv(inv) ## store the inverse in the cache
    inv ## return the inverse
    
}
