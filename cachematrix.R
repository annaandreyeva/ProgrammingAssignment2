## The pair of functions (makeCacheMatrix & cacheSolve) cache the inverse of a matrix.
## To test, run: 
## m <- matrix(c(3, 4, 5, 6), 2,2)
## x<- makeCacheMatrix(m)
## cacheSolve(x)

## makeCacheMatrix creates a special "matrix" object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set <- function (y) {
    x<<-y
    m<<-NULL
    
  }
  
  get <- function () x
  setinverse <- function (inverse)  m<<-inverse
  getinverse <- function () m
  list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}



## cacheSolve returns a matrix that is the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, cacheSolve returns the inverse from the cache

cacheSolve <- function(x, ...) {
         m <- x$getinverse()
     if(!is.null(m)) {
           message("getting cached data")
             return(m)
       }
    data <- x$get ()
     m <- solve(data, ...)
     x$setinverse(m)
   m
}
