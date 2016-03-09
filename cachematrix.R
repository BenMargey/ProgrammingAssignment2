## Put comments here that give an overall description of what your
## functions do

## Through this function the user can define a matrix and store the inverse of it in a cache independent of the environment

makeCacheMatrix <- function(x = matrix()) {
        iv <- NULL
        set <- function(y) {
                x <<- y
                iv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) iv <<- inverse
        getInv <- function() iv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}



## This function the cached inverse of the matrix, or solves the matrix if it has not been already defined

cacheSolve <- function(x, ...) {
        #return a matrix that is the inverse of x
        iv <- x$getInv()
        if(!is.null(iv)) {
                message("getting cached data")
                return(iv)
        }
        mat <- x$get()
        iv <- solve(mat, ...)
        x$setInv(iv)
        iv
}
