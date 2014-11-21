## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<-matrix()
  solved<-FALSE
  set <- function(y) {
    x <<- y
    inv<<-matrix()
    solved<<-FALSE
  }
  get <- function() x
  setinv <- function(inv_mat) {
    solved<<-TRUE
    inv <<- inv_mat
  }
  issolved <- function() solved   
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv, issolved=issolved,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve<-function(x,...) {
  inv <- x$getinv()
  data <- x$get()
  if( identical(x$get(),data) & x$issolved() )  {
    message("getting cached data")
    return(inv)
  }
  
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
