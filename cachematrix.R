## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
         matrix <- NULL
         set <- function(y) {
                x <<- y
                matrix <<- NULL
        }
        get <- function() x
        getInverseMatrix <- function() matrix <<- solve(x)
        setInverseMatrix <- function(invertMat) matrix <<- invertMat
        list(set = set, get = get,
                setInverseMatrix = setInverseMatrix,
                getInverseMatrix= getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        matrix <- x$getInverseMatrix ()
        if(!is.null(matrix)) {
                message("getting cached data")
                return(matrix)
        }
        data <- x$get()
        matrix <- solve(data)
        x$setInverseMatrix(matrix)
        matrix
}
