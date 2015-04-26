# To cache the inverse of a matrix rather than compute it repeatedly use the following functions.
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(u = matrix()) {
        i <- NULL
        set <- function(v) {
                u <<- v
                i <<- NULL
        }
        get <- function() u
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
# The following function returns the inverse of the matrix. 
cacheSolve <- function(u, ...) {
        i <- u$getinverse()
        if(!is.null(i)) {
                return(i)
        }
        data <- u$get()
        i <- solve(data)
        u$setinverse(i)
        i
}
## Sample run:
u <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
mat2 = makeCacheMatrix(u)
mat2$get()
## [,1] [,2]
## [1,] 1.00 -0.25
## [2,] -0.25 1.00
## No cache in the first run
cacheSolve(mat2)
## [,1] [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## Retrieving from the cache in the second run
cacheSolve(mat2)
## getting cached data.
## [,1] [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 
