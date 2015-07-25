## Documentation style loosely based on:
##  https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml

# Matrix inversion can be computationally expensive.
# In some cases, caching the inverse of a matrix may
#  yield improved runtimes.

# To facilitate this, two cooperating functions are
# provided:
# makeCacheMatrix(x)
# - create an "object" which supports caching of its
#   inverse
# cacheSolve(cacheMatrix)
# - solver that first checks whether there's an already
#   computed solution cached in the object

# Example of use:
# > x <- makeCacheMatrix(matrix(1:4,2,2))
# > cacheSolve(x) # first time requires computation
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(x) # second time uses the cache
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > x$set(matrix(0:3,2,2))
# > cacheSolve(x) # new data invalidates the cache
#      [,1] [,2]
# [1,] -1.5    1
# [2,]  0.5    0
# > cacheSolve(x) # cache populated for the 2nd call
# getting cached data
#      [,1] [,2]
# [1,] -1.5    1
# [2,]  0.5    0


makeCacheMatrix <- function(x = matrix()) {
  # Creates an "object" and exposes methods" to
  #  maintain a matrix and lazily-cached inverse.
  # The state is actually held in an environment
  #  common to all the accessors / mutators.
  # The "methods" are maintained in a list.
  #
  # Args:
  #  x: Initial matrix to use for the internal state.
  #      (defaults to an empty matrix)
  #
  # Returns:
  #  List containing accessor / mutator functions:
  #    set(x) - set the matrix data
  #    get()  - get the matrix data
  #    setinverse(x) - (private) set the inverse
  #    getinverse()  - (private) get the last cached inverse
  #
  
  # Use NULL as a placeholder indicating "not computed yet".
  inverse <- NULL
  
  # set(y) method to accept new matrix data
  set <- function(y) {
    # set the matrix data
    x <<- y
    # invalidate the cache
    inverse <<- NULL
  }
  
  # get() method for fetching matrix data
  get <- function() x
  
  # setinverse(newInverse)
  # - method for caching the computed inverse
  # - should only be called by cacheSolve(x) !
  setinverse <- function(newInverse) inverse <<- newInverse

  # getinverse()
  # - method for retrieving the cached inverse
  # - should only be called by cacheSolve(x) !
  getinverse <- function() inverse
  
  # bundle the related methods together in a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'

  # Get the cached inverse.
  inverse <- x$getinverse()
  
  # See if it's something other than the NULL placeholder.
  if(!is.null(inverse)) {
    message("getting cached data")
    # Not null.  Return it to the user.
    return(inverse)
  }
  # Need to compute the inverse since it's not cached.
  # Get the underlying matrix data.
  data <- x$get()
  # Compute the inverse.
  inverse <- solve(data, ...)
  # Stash the computed inverse in the cache so it's
  #  available for immediate use next time.
  x$setinverse(inverse)
  
  # Return the result to the user.
  inverse
}
