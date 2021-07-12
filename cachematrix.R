## Functions that cache the inverse of a matrix 

## To create a cached matrix to get and set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        matrix(c(set = set, get = get, setinverse = setinverse, getinverse = getinverse), 2,2)

}


## Function to calculate the inverse of the matrix created above and caching it 
## If there is a cached value of the inverse then it is not computed again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x[2,2]
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x[1,2]
        inv <- solve(data, ...)
        x[2,1](inv)
        inv
        
}
