## Put comments here that give an overall description of what your
## functions do

## special matrix type that can stores the inverted matrix
##

makeCacheMatrix <- function(x = matrix()) {
  invMatr <-NULL
  set <- function(y) {
      x <<- y
      invMatr <<- NULL
    }
  get <- function() x
  setInvMatr <- function(matI) invMatr <<- matI
  getInvMatr <- function() invMatr
  list(set = set, get = get,
             setInvMatr = setInvMatr,
             getInvMatr = getInvMatr)
   

}


## the function computes the inverted matrix, if nothing is in cache 
## otherwise it returns the cached matrix
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x' 
    m<-x$getInvMatr()
    if(!is.null(m)) {
        message("getting cached inverted matrix")
        return(m)
    }
    data <-x$get()
    m<-solve(data)
    x$setInvMatr(m)
    m
}
