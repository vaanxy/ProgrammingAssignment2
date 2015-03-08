## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set     : set the value of the matrix
##get     : get the value of the matrix
##setSolve: set the inverse of the matrix
##getSolve: get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function (y) {
                x <<- y
                s <<- NULL
        }
        get <- function () x
        setSolve <- function (solved) s <<- solved
        getSolve <- function () s
        list (
                set = set,
                get = get,
                setSolve = setSolve,
                getSolve = getSolve
             )

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached inversed matrix")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}
