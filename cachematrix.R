## Functions makeCacheMatrix and cacheSolve cache the inverse of matrix.
## We assume that the matrix supplied is always invertible.

## Function makeCacheMatrix creates list containing a function to:
## set the value of the matrix,
## get the value of the matrix,
## set the value of the inverse of matrix
## and get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) { 
        inversematrix <- NULL
        set <- function(y) { 
                x <<- y
                inversematrix  <<- NULL
        } 
        get <- function() x 
        setinverse <- function(solve) inversematrix <<- solve
        getinverse <- function() inversematrix
        list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
}


## Function cacheSolve creates inverse of matrix.
## First it checks if inverse of matrix has already been calculated.
## If so, it gets the inverse of matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse of matrix and sets it in the cache via the setinverse function.


cacheSolve<- function(x, ...) {
        inversematrix <- x$getinverse ()
        if(!is.null(inversematrix)) {
                message("getting cached inverse of matrix")
                return(inversematrix)
        }
        data <- x$get()
        inversematrix <- solve(data, ...)
        x$setinverse(inversematrix)
        inversematrix
}
