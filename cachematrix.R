## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## Set the value of matrix
        i<- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
}
        ## Get the value of matrix
        get <- function() x
        ## Set the inverse of matrix
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        ## Get the inverse of matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Get the inverse of matrix
        i <- x$getinverse()
        ## Make sure if there's the matrix
        if(!is.null(i)) {
               message("getting cached data")
               return(i)
        }
        ## If there's not the matrix, Get the inverse of matrix
        data <- x$get()
        i <- solve(data, ...)
        ## Set the inverse of matrix
        x$setinverse(i)
        i
}
