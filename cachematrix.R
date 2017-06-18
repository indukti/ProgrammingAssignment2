## The functions are used to store the input matrix x, calculate its inverse and cache the result
## so that it is not required to calculate it again.


## The function implements and returns four functions used to store/cache matrices x and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## It creates a new variable used to store the result of the matrix inverse operation, initially NULL value is assigned.
    i <- NULL
    
    ## The function is used to set new initial values - the input matrix x and empty (NULL) inverse matrix.
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## It returns the input matrix x.
    get <- function() x
    
    ## It stores the inverse of matrix x in the variable i.
    setInverse <- function(inverse) i <<- inverse
    
    ## It returns the cached value of the matrix which is the inverse of matrix x.
    getInverse <- function() i
    
    ## The list of functions implemented above is returned.
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The function calculates the inverse of a matrix which is created by using makeCacheMatrix function
cacheSolve <- function(x, ...) {
    ## It gets the current result stored in x variable.
    i <- x$getInverse()
    
    ## If the result has already been cached return it - there is no need to calculate it again.
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## If the result has not been cached get the initial matrix x and calculate its inverse matrix by using solve function.
    matrix <- x$get()
    m <- solve(matrix, ...)
    
    ## It stores the result in x variable.
    x$setInverse(m)
    m
}