## These functions allow to create a special matrix, calculate its inverse 
## and retrieve the inverse without calculating it every time it is needed.

## This function creates a list of functions that work on the 
## given matrix x (which can be unspecified). Specifically 
## they allow to get or set the matrix x, check if the inverse matrix
## has been calculated and retrieve the inverse matrix.

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


## This function calculates the inverse of the matrix saved 
## using the makeCacheMatrix function. If the inverse has been already 
## calculated the inverse is simply retireved from memory and not re-calculated.

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
