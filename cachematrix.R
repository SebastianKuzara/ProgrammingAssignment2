## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function create a list containing 4 functions
makeCacheMatrix <- function(x = matrix()) {
   
   ## Assign "NULL value" to the s variable that stored the inverse matrix
    s <- NULL
    
    ## "set" function allows to enter matrix to cache
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    ## The following function is used to get matrix from memory 
    get <- function() x
    
    ## By "setsolve" function we can enter inverse matrix to cache
    setsolve <- function(solve) s <<- solve
    
    ## "getsolve" function is used to get inverse matrix from cache
    getsolve <- function() s
    
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function

## This function calculetes the inversion of matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
    ## An attempt to load inverse matrix from cache     
    s <- x$getsolve()
    
    ## If there is inverse matrix, function will get it from cache
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    ## If there isn't inverse matrix in cache, 
    ## program compute it by the solve function 
    ## for specified matrix in the list created by "makeCacheMatrix" function
    data <- x$get()
    s <- solve(data)
    
    ## Next, inverse matrix is replaced in cache in that list
    x$setsolve(s)
    s
}
