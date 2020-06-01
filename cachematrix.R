## Week 3 project of R programming
## A pair of R functions that can cache time-consuming computation like
## matrix inversion


## This function create a matrix object that can cache its inverse

makeCacheMatrix <- function (x= matrix()){
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inver) m <<- inver
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the matrix returned by the function 
## above.If the inverse has been calculated and is not changed, then retrieve the
## inverse from the cache.

cacheSolve <- function (x,...){
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}


## test the functions using simple matrix

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1
m2 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
m2

mym<-makeCacheMatrix(m1)

mym$get()

mym$getinverse()
cacheSolve(mym)

mym$set(m2)
cacheSolve(mym)