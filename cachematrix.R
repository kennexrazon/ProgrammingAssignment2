## Generate the inverse of the matrix

## makeCacheMatrix - takes a matrix x and returns a list of objects cached in memory
##
## usage: makeCacheMatrix(x)
##
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
		## store data outside the function enviroment
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
		## store the solve function as an object outside the function enviroment
        setinv <- function(solve) i <<- solve
        getinv <- function() i
		## aggregate the objects for easier access
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve - takes x, a list of objects produced by makeCacheMatrix, and returns
## the inverse of the matrix from the list x. The matrix inverse is stored in cache 
## if it isn't computed yet. 
##
## usage: special_matrix <- makeCacheMatrix(x)
##        cacheSolve(special_matrix)
##
## usage: cacheSolve(makeCacheMatrix(x))
##
cacheSolve <- function(x, ...) {
        ## take the function for solving the matrix inverse
		i <- x$getinv()
		## check if the inverse of the matrix has already been solved
        if(!is.null(i)) {
				## inform the user that the return value is from cache
                message("getting cached data")
				## return the cached matrix inverse
                return(i)
        }
		## compute the inverse of the matrix
        data <- x$get()
        i <- solve(data, ...)
		## store the inverse of the matrix to cache
        x$setinv(i)
		## return the inverse of the matrix
        i
}
