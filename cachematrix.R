# Function that creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  inversemat <- NULL
  
  # Declaring another function set for the value will to be cached 
  set <- function(y) {
    x <<- y
    
   # If the matrix was changed then change the value of inverse of the matrix.
   inversemat <<- NULL
  }
  
  # Gets the value of inverse
  get <- function() return(x);
  setinverse <- function(inv) inversemat<<- inv;
  getinverse <- function() return(inversemat);
  return(list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse))
}

# The function below calculates the inverse of matrix returned by makeCacheMatrix
# Firstly it would check to see if the inverse has already been calculated. 
# If yes, it would skip the computation and gets the inverse matrix from the cache. 
# Else it uses solve function to computethe inverse of a square.

cacheSolve <- function(x, ...) 
  {
       
  inv <- x$getinverse()
  
  # Gets inverse if it exist.
  if(!is.null(inv)) 
    {
    message("retrieving the cached data")
    return(inv)
  }
  
  # Calculate the matrix and inverse it, if the inverse matrix does not exist. 
  data <- x$get()
  
  # Use solve function to compute the inverse of a square matrix.
  inv <- solve(data, ...)
  x$setinverse(inv)
  return(inv)

  
}
