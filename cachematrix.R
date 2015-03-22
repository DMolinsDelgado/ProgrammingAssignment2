## The following functions cache the inverse of a matrix, saving 
## costly computations.

## makeCacheMatrix creates a list that sets and gets values for the 
## matrix and sets and gets values for its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<--y
        m<<-NULL
    }
    get<-function()x
    setinverse<-function(inverse)m<<-inverse
    getinverse<-function()m
    list(set=set,get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}

## cacheSolve returns the inverse of a matrix, checking if the 
## matrix has been already computed. If so, immediately after
## returns the result. If not, performs the inversion and returns 
## the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', assuming 
        ## always that is invertible.
    m<-x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setinverse(m)
    m
}
## test run:
## > ma<-matrix(1:4,2,2)
## > mx<-makeCacheMatrix(ma)
## > mx$get()
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(mx)
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
