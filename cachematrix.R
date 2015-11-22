## These two functions allow the cacheing of and solving for the inverse
## of a matrix.

## This function caches the given matrix to be used in the function below

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL

        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        get <- function() x

        setmean <- function(mean) m <<- mean

        getmean <- function() m

        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)


}


## This function computes the inverse of the matrix created in the above
## function; if the inverse has already been calculated and the given
## matrix hasn't changed this function will retrieve the inverse from
## the cache

cacheSolve <- function(x, ...) {

        m <- x$getmean()

        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        data <- x$get()

        m <- mean(data, ...)

        x$setmean(m)

        solve(data)

}
