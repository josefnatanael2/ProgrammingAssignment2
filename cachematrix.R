## Caching the inverse of a matrix
## If inverse have been calculated, obtain previous computation

## A special type of Matrix, that stores some methods
## @params x = matrix() is an R matrix
## returns a list of functions, i.e. set, get, setinverse, getinverse
## the setinverse method sets the inverse matrix for the cacheMatrix
## the getinverse method retrieves the inverse matrix for the cacheMatrix
## set and get are respectively mutator and accessor methods for the matrix data
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Inverse a special matrix, if not previously computed
## @params x cacheMatrix upon which to perform inverse, if inverse not previously computed
## @params ... other arguments passed to solve()
## returns the inverse of the cacheMatrix. If the input cacheMatrix has an inversematrix data,
## this function will simply return the cached inverse data of the input cacheMatrix
## Otherwise, the inverse of the input cacheMatrix will be passed to the solve() function 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
