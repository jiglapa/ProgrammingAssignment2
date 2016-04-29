## First cache a matrix
## Then computes its inverse using cache for optimizing calculation


## This a function to cache a matrix
## @x is a a square invertible matrix
## return: a list of four functions to
##  1. Set the matrix
##  2. Get the matrix
##  3. Calculate / set the inverse matrix using solve()
##  4. Get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y){
    ## Use <<- to assign a value to an object in a different enviroment
    ## fron the current one
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setmatrix <<- function(solve) inv <<- solve
  getmatrix <<- function() inv
  
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
  

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cacheSolve should retrieve the inverse from the cache
##  @x a square invertible matrix output of makeCacheMatrix()
##  return: thne inverse matrix of the original input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
        
  
  inv <- x$getmatrix()
  
  ## If the inverse matrix has been already calculated
  
  if(!is.null(inv)){
    ## Inform and skip calculation
    message("getting cached data")
    return(inv)
  }
  
  ## Otherwise, make the calculation
  data <- x$get()
  inv <- solve(data, ...)
  x$setmatrix(inv)
  return(inv)
}
