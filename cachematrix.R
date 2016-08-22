## Caching the Inverse of a Matrix

## Function that creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
    x <<- y
    inv <<- NULL
}
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Function that computes the inverse of a special matrix (above) If the inverse has already been calculated then this function should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data!")
        return(inv)
    }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
    }
## Test Run
## > x = matrix(1:4, 2, 2)
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## No cache in the test run

## > cacheSolve(m)
## getting cached data!
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## getting cached data!
## Retrieved from the cache

##> m$getinverse()
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## > inv <- NULL