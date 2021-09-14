## Cached value of Matrix inversion
## Example Input:
##      source("cachematrix.r")
##      mm <- matrix(c(1,3,2,4),nrow=2,ncol=2)
##      data <- makeCacheMatrix(mm)
##      invmat <- cacheSolve(data)



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function (x = matrix()) {
        Mat <- NULL
        set <- function (y) {
                x <<- y
                Mat <<- NULL
        }
        get <- function () x
        setinv <- function(solve) Mat <<- solve
        getinv <- function () Mat
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from the
## cache.

cacheSolve <- function(x, ...) {
        M <- x$getinv()        
        if(!is.null(M) | all(M == x$get())){       #(all() checks all values)
                message("getting cached matrix")
                return(M)
        }
        data <- x$get()
        M <- solve(data)
        x$setinv(M)
        M
}