## Matrix inversion is a potentially time consuming computation which may benefit
## from the creation of a cache to improve time efficiency.
## makeCacheMatrix creates a list containing a function to
## matrix object that can cache its inverse.
## The following functions assume that a given matrix is invertible.
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

## cacheSolve returns the inverse of matrix 'x'.  It initally checks whether or not
## a specific inverse has already been computed, and returns that value if TRUE.
## If not, it computes the inverse and sets it to the cache.
cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if(!is.null(inv)) {
           message("getting cached data.")
           return(inv)
     }
     data <- x$get()
     inv <- solve(data)
     x$setinverse(inv)
     inv
}
