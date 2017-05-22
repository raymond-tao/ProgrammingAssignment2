makeCacheMatrix <- function(x = matrix()) { 
  ## use this function to create a list of functions from a matrix
  ## for example:  a<- makeCacheMatrix(matrix(rnorm(100),c(10,10)))
  ## then pass it to the cacheSolve function
  m <- NULL     
  set <- function(y)
  { 
    x <<- y
    m <<- NULL 
  }  
  get <- function() {x} 
  setsolve <- function(solve) {m <<- solve}  
  getsolve <- function(){m}
  
  #return a list of functions
  list(
    set = set, 
    get = get,    
    setsolve = setsolve,       
    getsolve = getsolve) 
} 


cacheSolve <- function(x, ...) {
  ##use this function to invert the matrix 
  ##if already exists, retrun the cached data
  ##for example: cacheSolve(a)
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##otherwise, get the matrix and invert it
  ##once inverted, the result will be stored for next time
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}





