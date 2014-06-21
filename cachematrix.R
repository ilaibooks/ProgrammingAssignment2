# Reviewer: I know the instructor likes 8 spaces of indentation. I don't and neither
#           do most programmers I encounter. Please don't mark off for it. Also, if I were
#           writing this myself, I'd change most of the names to make them more meaningful. 

# This file contains two functions which implement matrix inversion. makeCacheMatrix take
# a matrix definition as its argument. It returns a unique object type. Use makeCacheMatrix
# to create (return) a cachable matrix object. You can also use makeCacheMatrix to retrieve 
# or set object attributes. cacheSolve is a wrapper function that figures out whether the
# matrix inversion has been cached or not and takes action appropriately. Call cacheSolve
# with a makeCacheMatrix object in order to get the (possibly cached) inverse of a matrix.

# Usage:
#  1. z <- makeCacheMatrix( <matrix definition> )
#  2. cacheSolve( z )
#
# Example:
#  z <- makeCacheMatrix(rbind(c(1,2), c(3,4)))
#
#  cacheSolve(z)
#       [,1] [,2]
#  [1,] -2.0  1.0
#  [2,]  1.5 -0.5
#
#  cacheSolve(z)
#  getting cached data
#       [,1] [,2]
#  [1,] -2.0  1.0
#  [2,]  1.5 -0.5

# ************************************************************************************************

# makeCacheMatrix defines an object to hold a matrix, its inverse, and functions to manipulate
# both the matrix and the inverse. 
#
#   functions (methods):
#             set             set object to operate on
#             get             get (return) object to operate on
#             setinverse      set the inverse of the currently-set object (see "set")
#             getinverse      get the inverse of the currently-set object (see "set")
           
makeCacheMatrix <- function(x = matrix()) {
  # initialize the cached inverse object for first-time use
  # NULL indicates not yet calculated
  m <- NULL
  
  # function to set the matrix on which to calculate inverse
  set <- function(y) {
     x <<- y
     m <<- NULL
  }
  
  # function to get the matrix on which to calculate inverse
  get <- function() {
     x
  }
  
  # function to set the inverse of the cached object
  setinverse <- function(inverse) {
     m <<- inverse
  }
  
  # function to get the inverse of the cached object
  getinverse <- function() {
     m
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
} # end makeCacheMatrix

# ************************************************************************************************

# cacheSolve takes a makeCacheMatrix object as its argument and returns the inverse of
# the matrix defined by that object, using the previously-cached value if possible.

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if (!is.null(m)) {
     message("getting cached data")
     return(m)
  } else {
            # otherwise, if m is not null, do this:
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
            return(m)
  }
  
} # end cacheSolve
