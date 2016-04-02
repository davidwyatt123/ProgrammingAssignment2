#Here we are going to build a function that will create a matrix that is a list
# containing a function to set and get the value of the matrix, then get and set its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) inv <<- Inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# In the next section of code we will compute the inverse of the matrix created with above function,
# But before doint that, we will check to see if this inverse matrix has already been solved and stored,
# if yes, it will retrieve the inverse matrix from cache memory.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

# to get this to work, type
# > a <- makeCacheMatrix()
# > a$set(matrix(1:4,2,2))
# > cacheSolve(a)
# [1] inital answer shown here
# > cacheSolve(a)
# getting cached data
# [1] cached answer shown here