## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Creates a "matrix" object that is able to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #Initial matrix value
  set <- function(y) { #Set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x #Get the value of the matrix
  setInverse <- function(solve) m <<- solve #Function that uses the
  #solve function to get the inverse of the matrix
  getInverse <- function() m #Gets the value of the matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#This function either computes the inverse of the "matrix" or returns the cached 
#inverse if the inverse has already been calculated.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse() #Get the inverse
  if(!is.null(inv)) { #If there is an inverse in the cache memory
    message("Getting inverse of matrix") 
    return(inv) #Return the inverse
  }
  data <- x$get() #Get the value of the matrix
  inv <- solve(data, ...) #Calculate the inverse
  x$setInverse(inv) #Set the cache value
  inv 
}

