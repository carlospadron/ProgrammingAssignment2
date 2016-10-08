## Creates a cache of the matrix and its inverse to avoid recalculation

## Creates cache

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        #sets y in the global environment and resets the inv variable 
        set<-function(y){
                y<<-x
                inv<<-NULL
        }
        #returns x
        get<-function() x
        #sets the inv variable 
        setinverse<-function(inverse) inv<<-inverse
        #returns the inv variable
        getinverse<-function() inv
        #finally a list of the two variables gets returned
        list(set = set, get=get,setinverse = setinverse,getinverse=getinverse)
}

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        #gets the cached inverse
        i<-x$getinverse()
        ## checks if inverse is null
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}