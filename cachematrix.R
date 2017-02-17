## Put comments here that give an overall description of what your
## functions do
## These two fucntions cache the inverse of a matrix.
## For example: 
## Suppose we define a matrix: x <- matrix(1:4, nrow=2, ncol=2)
## Then we do: cacheMatrix <- makeCacheMatrix(x) 
## Call: cacheSolve(cacheMatrix) gives the result 
##
##       [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
#Since this is the first time we calculate the inverse of the matrix, we simply caluclate;
#Then this inverse will be cached to cacheMatrix, and can be got by cacheMatrix$getinv()


## Write a short comment describing this function
## This function creates a special list (it can be regarded as a special data structure), containing
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(m_inv) inv <<- m_inv
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## This function returns the inverse of a matrix. If this inverse already exists, then get the cached inverse. 
## Otherwise, calculate the inverse, and cache it into the data structure in "makeCacheMatrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
            message("getting cached data")
            return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix)
        x$setinv(inv)
        inv
}
