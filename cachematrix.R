## These functions work together to calculate and store the inverse of a square matrix.

## makeCacheMatrix creates a special matrix object that can cache its inverse. When needed,
## the inverse can be retrieved from the cache without repeating a potentially costly computation.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL               # preallocate variable m
        set <- function(y) {
               x <<- y          # superassignment operator takes the argument to set() and writes it "upstairs" to
                                # the variable x (the argument to makeCacheMatrix) in the parent environment
               m <<- NULL       # superassignment operator nulls m in the parent environment.Needed to clear cached m 
                                # if set() is used
        }
        get <- function() x     #get() gets x (the original matrix)
        setinverse <- function(inverse){
                m <<- inverse   # this remains NULL until cacheSolve is called. Once it is called, this writes the
                                #inverse matrix "upstairs" to the variable m in the parent environment
        }
        getinverse <- function() m #get the inverse matrix stored in m (or return NULL if cacheSolve hasn't yet run)
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse) #return the four functions in a list
}

## cacheSolve looks for an inverse matrix stored the special matrix object returned by makeCacheMatrix.
## If it finds one, it returns it, otherwise it retrieves the original matrix and calculates its inverse.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()     # look for the inverse in makeCacheMatrix
        if(!is.null(m)) {       # if m already exists, return the message, then return m.
                message("getting cached data")
                return(m)
        }                       #otherwise...
        data <- x$get()         #get the matrix
        m <- solve(data, ...)   #calculate the inverse matrix
        x$setinverse(m)         #store the inverse matrix in the variable m in the environment of makeCacheMatrix
        m                       #return the inverse matrix
}
