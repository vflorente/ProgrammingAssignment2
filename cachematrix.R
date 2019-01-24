## Cached Matrix Inversion
## The purpose of the functions is to cache the inversion 
## of a matrix instead of having to compute it repeatedly 

## This function creates a cache matrix from a given matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(inverse) m <<- inverse
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## This function returns the inverse of a given matrix that's cache

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setmatrix(m)
        return(m)
}

## Below Tests Functions:

## Set Matrix
x = rbind(c(4, 7), c(2, 6))
m = makeCacheMatrix(x)

## Display Matrix
m$get()

## No cache in the first run
cacheSolve(m)

## Display message "getting cached data"
## Retrieving from the cache in the second run
cacheSolve(m)