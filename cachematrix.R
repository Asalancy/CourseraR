## These two functions invert a matrix

makeCacheMatrix <- function(x = matrix()) {
      
      ## Assign function makeCacheMatrix to a variable (call it myMatrix) with the matrix to be inverted passed as argument.
      
      ##  makeCacheMatrix keeps track of whether the inverted matrix has already been created by setting the variable m.
      m <- NULL
      ##  The set function allows for passing a new matrix through myMatrix$set.
      ##  and resets m to NULL to indicate cacheSolve must solve for inversion.
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      ##  The get function is necessary to pass the matrix to the solve function call in cacheSolve.
      get <- function() x
      ##  getinvert and setinvert hold the matrix at the parent level, for cacheSolve to access.
      setinvert <- function(invert) m <<- invert
      getinvert <- function() m
      ## assigning the list members to the function names allows for them to be called by $set, $get, etc.
      list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)
}

cacheSolve <- function(x, ...) {

      ## Return a matrix that is the inverse of 'x'
      ## cacheSolve checks to see if the inverted matrix has already been calculated
      m <- x$getinvert()
      if(!is.null(m)) {
         message("getting cached data")
         return(m)
            ## The return call ends the function by returning the matrix from the parent level, avoiding the matrix inversion calculation below
      }
      data<-x$get()
      ## I'm still not entirely certain why this is necessary, as I thought x would be available given its definition at the parent level
      ##  but leaving this out and simply calling solve(x,...) doesn't work, as it doesn't recognize x as a matrix
      m <- solve(data,...)
      x$setinvert(m)
      m
}
