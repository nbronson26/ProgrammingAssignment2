## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## m<-Null = placeholder for future value
## Set<-function = defines a function to set the matrix, x, to a new matrix
## get<-function = returns the matrix, x
## setmean = sets the matrix, m, to matrix
## getmean = returns the matrix, m
## list = returns the 'special vector' containing all of the functions just defined

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setinverse<-function(solve) m<<- solve
    getinverse<-function() m
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## Write a short comment describing this function
## cacheSolve = Computes, caches, and returns matrix inverse
## m<-x$getinverse = Returns matrix inverse
## if = m(x$getinverse) is not null, return matrix inverse using cached data
## x$get() = retruns matrix
## x$setinverse = returns matrix inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setinverse(m)
    m
}
}
