## The functions will allow to get the inverse value of the cached matrix. 
## In case that the previous matrix was already stored, the function will return cached inverse.

## The function will allow to cache the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {

  # Create the property of inversion
  i <- NULL
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL}
  
  # Get the matrix
  get <- function() x

  # Method to set the inverse of the matrix
  setInverse <- function(inverse) i <<- inverse
  
  # Method to get the inverse value of the matrix
  getInverse <- function() i
  
  # Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## The following function will compute the inverse of the matrix. In case that the inverse value was already computed
## It will return the cached value

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  
  i <- x$getInverse()
  
  # Return the inverse if it was already calculated
  if(!is.null(i)) {
    message("Return cached data")
    return(i)
  }
  
  # Get the matrix from 'x'
  data <- x$get()
  
  ## Calculate the inverse of the matrix
  i <- solve(data, ...)
  
  ## Set the inverse to the 'x'
  x$setInverse(i)
  
  ## Return the matrix
  i
}

## Check the program
a1 <- c(3, 2, 5)
a2 <- c(2, 3, 2) 
a3 <- c(5, 2, 4) 

# bind the three vectors into a matrix  
# using rbind() which is basically 
# row-wise binding.

A <- rbind(a1, a2, a3)
test2 <- makeCacheMatrix(A)
cacheSolve(test2)
