## Put comments here that give an overall description of what your
## functions do

## This function creates a special "vector" that contains 4 functions
## 1. set(): set the matrix
## 2. get(): get the matrix
## 3. setinv(): set the inverse of the matrix
## 4. getinv(): get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function () inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes in an object that is the result of makeCacheMatrix, 
## then check if the inverse of the matrix has been calculated and cached.
## If so, a message is displayed and the cacahed matrix inverse is returned;
## If not, the matrix inverse is calculated and cached, then returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached matrix inverse")
        return(inv)
    }
    m <- x$get()
    inv <- solve(m)
    x$setinv(inv)
    inv
}

## test sample
## ma <- makeCacheMatrix(matrix(c(4,3,3,2), 2, 2))
## cacheSolve(ma)