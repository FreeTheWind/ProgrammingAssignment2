## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL       # set the inv as NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x           # get matrix function
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv        # get inverse function
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {                    # check the inv is NULL or not
      message("getting cached data")
      return(inv)
    }
    invmatrix <- x$get()
    inv <- solve(invmatrix, ...)           # solve the inverse value
    x$setinv(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
## Testing the code
m1 <- makeCacheMatrix(matrix(c(2,3,6,5,9,8,10,15,22), 3, 3))
m1$get()
m1$getinv()
cacheSolve(m1)
