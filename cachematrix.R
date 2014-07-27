# This functions create a special "matrix" object 
# that can cache its inverse and a function that 
# returns the inverse of the matrix, looking up
# for the cached inverse when available.j

# The code assumes the input for makeCacheMatrix
# is an invertible matrix

# The special "matrix" is really a list containing
# functions to:
# 1: 'set': set the matrix value, eg: <specialMatName>$set(matrix(1:4,2,2))
# 2: 'get': get the matrix value, eg: <specialMatName>$get()
# 3: 'setInv': sets the value of the matrix Inverse in memory
# 4: 'getInv': gets the value of the precalculated matrix inverse

##makeCacheMatrix()
## This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(invMat) inv <<- invMat
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}
}


##cacheSolve()
# This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
