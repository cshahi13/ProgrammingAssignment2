
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## The folllowing function creates a special "matrix" object that can cache its inverse.
## Here matrix is always square matrix.


## The makeCacheMatrix function, creates a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
                inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(mean) inverse <<- mean
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the matrix created with the above function.
## However, it first checks to see if the inverse of the matrix has already been calculated.
## If so, it gets the  inverse of the matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix  and sets the value of the inverse
## of the matrix  in the cache via the setmean function.


cacheSolve <- function(x, ...) {
               inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
        ## Return a matrix that is the inverse of 'x'
        
}

## Sample Run
## > x = matrix(1:4, 2, 2)
## > inverse = makeCacheMatrix(x)
## > inverse$get()
##     [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(inverse)
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(inverse)
## getting cached data.
 ##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
