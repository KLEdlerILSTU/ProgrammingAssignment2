## When the user creates a variable that calls the makeCacheMatrix function
## (which has been passed a user-defined matrix), the variable contains
## the user-defined matrix, as well as a list of functions. When
## the cacheSolve function is called and passed the variable as an argument
## it can evaluate whether the inverse matrix exists, and if not, 
## calculate the value and store it so it can be used without
## re-doing the calculation.

## The makeCacheMatrix function takes a user-defined matrix and
## returns a list containing four functions: 1) set - to store 
## the user-defined matrix as the one used in the solve calculation; 
## 2) get - to return the current value of the set matrix;
## 3) setinverse.matrix - to calculate and then store the value of 
## the inverse matrix; 4) getinverse.matrix - to return the value of
## the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inverse.matrix <- NULL
        set <- function(y) {
                x <<- y
                inverse.matrix <<- NULL
        }
        get <- function() x
        setinverse.matrix <- function(solve) inverse.matrix <<- solve
        getinverse.matrix <- function() inverse.matrix
        list(set = set, get = get,
             setinverse.matrix = setinverse.matrix,
             getinverse.matrix = getinverse.matrix)
}


## The cacheSolve function takes a user-defined matrix, and will return
## the inverse of the matrix (without further calculation) if it is known.
## If the value of the inverse matrix is NULL, then the calculation is
## performed before the inverse matrix is returned.

cacheSolve <- function(x, ...) {
        inverse.matrix <- x$getinverse.matrix()
        if(!is.null(inverse.matrix)) {
                message("getting cached data")
                return(inverse.matrix)
        }
        data <- x$get()
        inverse.matrix <- solve(data, ...)
        x$setinverse.matrix(inverse.matrix)
        inverse.matrix
}
