## Generate the Inverse of an Inversible Matrix

## Functions to Get,set the matix and Inverse matrix , Storing the Inv / value into cache.

makeCacheMatrix <- function(m = matrix())
{
  inv <- NULL
  getMat <- function() m
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  
  list(getMat = getMat,setInv = setInv,getInv = getInv)
}


## Checks if existing inverse of matrix is in the variable (cache), if it exists returns the value. 
## If not exists then using solve function, inverse is calculated and stored in variable (cache) for next fetching

cacheSolve <- function(m)
{
  inv <- m$getInv()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  orig <- m$getMat()
  inv <- solve(orig)
  m$setInv(inv)
  inv
}