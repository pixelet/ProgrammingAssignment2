## Part #1:
## Matrix inversion is a costly computation and there are benefits to caching the
## inverse of a matrix rather than computing it every time when used. These two functions
## that allow caching of matrix inverse calculation

## This function creates a special "matrix" object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## assuming a squre invertibale matrix
    inv <- NULL
    set <- function(y) {
    x <<- y
    inv <<- NULL
    ## 1. Setting the matrix and using '<<-' to assign a value to an object
    ## in an environment different from the current environment.
        
    }
    get <- function() x
    ## 2. Getting the matrix value
    setinv <- function(inverse) inv <<- inverse
    ## 3. Setting the cached inverse value
    getinv <- function() inv
    ## 4.Getting the cached inverse value
    list(set=set, get=get, setinv=setinv, getinv=getinv)
    ## 5.Returning a list consisting of our special functions
  

}


## Part#2:
## computes the inverse of the "matrix" returned by makeCacheMatrix().
## If the inverse has already been calculated and the matrix has not changed,
## it'll retrieve the inverse from the cache directly.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        ## Variable inv gets the value of inverse matrix
        if (!is.null(inv)){
              ## Checking to see if the inverse of 'x' exists
              message("getting cached data")
              return(inv)
        }
        
        data <- x$get()
        ## if not then calculate the inverse
        inv <- solve(data, ...)
        ## Storing value of inverse matrix in the cache of variable named "inv".
        x$setinv(inv)
        ## Returning inverse
        return(inv)
}

