## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly

## The makeCacheMatrix creates a special "vector", which really a list containing a function to
## 1. Set the value of the vector
## 2. Get the value of the vector
## 3. Set the value of the mean
## 4. Get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
            x <<- y
            inverse <<- NULL
        }
        get <- function() {
            x
        }
        setInverse <- function(inv) {
            inverse <<- inv
        }
        getInverse <- function() {
            inverse
        }
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}



# Notice that the funciton assumes the matrix is always invertible

# The function cacheSolve returns the inverse matrix. First of all, it checks
# if the inverse variable had been computed. If computed before, it will return the 
# cached inversed function. Otherwise, set the inverse in the cache via setInverse function

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
            message("Getting inversed cached data")
            return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        ## Return a matrix that is the inverse of 'x'
        inverse
}

