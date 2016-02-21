## The function create function who contains functions to manipulate matrix and
## get the inv. matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
         matrix <- NULL
         # set the base matrix
         set <- function(y) {
                x <<- y
                matrix <<- NULL
        }
        #get the base matrix
        get <- function() x
        #get the invert matrix
        getInverseMatrix <- function() matrix
        #set the invert matrix
        setInverseMatrix <- function(invertMat) matrix <<- invertMat
        # return the list
        list(set = set, get = get,
                setInverseMatrix = setInverseMatrix,
                getInverseMatrix= getInverseMatrix)
}


## This function compute the invert of a matrix
## We suppose that the matrix always has an invert matrix
## If the invert matrix has ever been computed, we cache the results to not compute ths function everytime we need it

cacheSolve <- function(x, ...) {
         # check if invert matrix has already been computed
         # if matrix is different from null, we get the cached results
         matrix <- x$getInverseMatrix ()
         if(!is.null(matrix)) {
                  message("getting cached data")
                  return(matrix)
         }
         # otherwise we start to compute the invert matrix with the solve() function
         # and set results
         data <- x$get()
         matrix <- solve(data)
         x$setInverseMatrix(matrix)
         matrix
}
