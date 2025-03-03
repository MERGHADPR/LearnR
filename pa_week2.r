makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  set <- function(y) {
    x <<- y  # Assign the new matrix to x in the parent environment
    inv <<- NULL  # Reset the inverse cache
  }
  get <- function() x  # Retrieve the current matrix
  setInverse <- function(inverse) inv <<- inverse  # Set the inverse in the cache
  getInverse <- function() inv  # Retrieve the cached inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Attempt to retrieve the cached inverse
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)  # Return the cached inverse
  }
  mat <- x$get()  # Fetch the matrix
  inv <- solve(mat, ...)  # Compute the inverse
  x$setInverse(inv)  # Store the inverse in the cache
  inv  # Return the computed inverse
}



#Teting of the two functions 
#Create a matrix
sample_matrix <- matrix(c(4, 7, 2, 3, 6, 1, 5, 8, 9), nrow = 3, byrow = TRUE)
# Initialize the cache with the sample matrix
cached_matrix <- makeCacheMatrix(sample_matrix)
# Compute and retrieve the inverse
inverse_matrix <- cacheSolve(cached_matrix)
print(inverse_matrix)
