## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                inversed <- NULL
                set <- function(y){
                  x <<- y
                  inversed <<- NULL
                }
                get <- function()x
                setInversedMatrix <- function(inverse)inversed <<-inverse
                getInversedMatrix <- function()inversed
                list(set = set, get = get,
                     setInversedMatrix = setInversedMatrix,
                     getInversedMatrix = getInversedMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
              inversed <- x$getInversedMatrix()
              if(!is.null(inversed)){
                message("getting chached data")
                return(inversed)
              }
              matrix <- x$get()
              inversed <- solve(matrix, ...)
              x$setInversedMatrix(inversed)
              inversed
        ## Return a matrix that is the inverse of 'x'
}
