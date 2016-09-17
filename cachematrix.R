# USAGE:
#   Load the source file into R with
# source("cachematrix.R")

#   Create a NUMERIC matrix, for example
# m=matrix(1:4,2,2)

# NOTE: we assume that the matrix is INVERTIBLE, otherwise the program will not work!

#   Create an object storing the result of makeCacheMatrix(m).
#   By definition of this function this object is a LIST OF 4 FUNCTIONS
# mylist = makeCacheMatrix(m)

#   Asks the inverse of m, or the cached inverse of m (if it has been already computed) with
# cacheSolve(mylist)
#   The first time of the call of cacheSolve(mylist), the mean is computed and returned, WITHOUT any print message
#   All the next times that cacheSolve(mylist) is called, a message will be printed saying
#   that the inverse already cached will be returned , instead of re-computing the inverse a second time

#   NOTE: if some objects of the matrix m were changed between a call to cacheMean(mylis) and the next one,
#   then we MUST manually update mylist using
# mylist$set(m)
#   This forces the inverse to be set again to NULL, so that when cacheSolve(mylist) is called again,
#   then the inverse is computed as if it was the first time (then cached for later use)

# returns a vector of 4 functions, to be later used by the function cacheSolve() as argument
makeCacheMatrix <- function(my_invertible_matrix=matrix()) {
  saved_inverse <- NULL
  
  # Sets the matrix my_invertible_matrix to a given matrix provided_matrix
  # Since the mean has not been computed yet, sets m to NULL # no return values
  set <- function(provided_matrix) {
    my_invertible_matrix <<- provided_matrix
    saved_inverse <<- NULL
  }
  # Note the use of <<- above (instead of <-)
  
  # Returns the matrix stored in my_invertible_matrix
  get <- function() my_invertible_matrix
  
  # Sets saved_inverse to the matrix computed_inverse (computed somewhere else) # no return values
  setinverse <- function(computed_inverse) saved_inverse <<- computed_inverse
  
  # Returns the inverse saved in  (or NULL if the mean has not been computed yet)
  getinverse <- function() saved_inverse
  
  # Returns a list of the 4 functions defined above
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Takes as input the vector list returned by the previous function.
# Returns the inverse of the matrix saved in that list (computing it if it has not been computed before)
cacheSolve <- function(vector_list) {
  computed_inverse <- vector_list$getinverse()
  
  if(!is.null(computed_inverse)) {
    message("getting cached data")
    return(computed_inverse)
  }
  
  my_invertible_matrix <- vector_list$get()
  
  computed_inverse <- solve(my_invertible_matrix)
  
  vector_list$setinverse(computed_inverse)
  
  # returns computed_inverse
  computed_inverse
}