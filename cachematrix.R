## Put comments here that give an overall description of what your
## functions do

## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inve <- NULL
        set <- function(y){
            x <<- y
            inve <<- NULL
        }
        get <- function() x
        setInve <- function(inverse1) inve <<- inverse1
        getInve <- function() inve
        list(set = set,
             get = get,
             setInve <- setInve,
             getInve = getInve)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inve <- x$getInve()
    if (!is.null(inve)) {
        message("getting cached one")
        return(inve)
    }
    mat1 <- x$get()
    inve <- solve(mat1, ...)
    x$setInve(inve)
    inve
}
