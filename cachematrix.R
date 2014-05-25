## Put comments here that give an overall description of what your
## functions do

## Creates a list of functions where set() stores matrix as x and clears cache i,
## get() returns matrix x, setInverse() sets i as the inverse of x, 
## getInverse() returns i

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL ## sets i to empty value
    set <- function(y) { ## funtion to store matrix y as x
        x <<- y ## creates parent variable x as y
        i <<- NULL ## clears cached value i when funtion makeCacheMatrix is called again
    }
    get <- function() x ## returns matrix x 
    setInverse <- function(inverse) i <<- inverse ## i to matrix inverse
    getInverse <- function() i ## returns matrix i
    list(set = set, get = get, ## creates list of funtions
         setInverse = setInverse,
         getInverse = getInverse)

}


## Inverses the matrix x or calls the cached value if already inversed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse() ## sets i to stored inverse value of x
    if(!is.null(i)) { ## checks to see if i has a value
        message("getting cached data")
        return(i) ## returns value of i
    }
    data <- x$get() ## gets matrix x
    i <- solve(data, ...) ## sets i to the inverse of x
    x$setInverse(i) ## calls fuction to store inverse of x
    i ## returns inverse
}
