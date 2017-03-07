## The first function, makeCacheMatrix creates a list containing a function to
##
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse matrix
##    get the value of the inverse matrix
##
## In other words, this function creates a special "matrix" object that can cache its inverse (if calculated by the cacheSolve function).

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversematrix <- function(inversematrix) m <<- inversematrix
        getinversematrix <- function() m
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
}



## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. A message "getting cached data" is displayed in this case.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinversematrix function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinversematrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinversematrix(m)
        m
}
