## Put comments here that give an overall description of what your
## functions do


## There are two functions in the file. The makeCacheMatrix function takes
# as an argument a matrix, and creates and returns a list of the
# following functions: 
# set = which sets the value of the matrix to the given matrix
# get = which returns the matrix 
# getinverse = which returns the saved or cached inverse matrix
# setinverse = which sets the value of the inverse matrix

# The second function in the file is cacheSolve which takes a matrix created
# by the makeCacheMatrix, and either returns the cached inverse matrix or
# finds the inverse and stores it in the makeCacheMatrix as well as returns it


## Write a short comment describing this function

# the function makeCacheMatrix takes a matrix as input and returns a list
# of four functions created in the function itself


makeCacheMatrix <- function(x = matrix()) { #takes a matrix as input

  i <- NULL               # variable to store the inverse 
  
  set <- function(y) {    # function set, which sets the value of the matrix
                          # and sets the variable i to NULL for a new matrix
    x <<- y
    i <<- NULL
  }
  
  get <- function() {     # function get, to return the value of matrix
    x
  }
  
  setinverse <- function(solve) i <<- solve   # function to find inverse and 
                                              # save it in the variable i
  
  getinverse <- function(){           # function to return the inverse 
    i
  } 
  
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)     # creating a list of all the four 
                                    # functions to return
  
}


## Write a short comment describing this function
# The function cacheSolve takes as an argument an object created by 
# makeCacheMatrix and returns the inverse of the matrix which was
# set in the makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()             # i stores the inverse of the matrix
  if(!is.null(i)) {               # if i had already been calculated
    message("getting cached data")
    return(i)                     # return the already calculated value
  }
  
  # if this is the first time calculating the inverse, ie, i is null 
  
  data <- x$get()             # store the matrix in data
  i <- solve(data, ...)       # store the inverse in i
  x$setinverse(i)             # set the calculated inverse in the matrix for 
                              # future use
  i                           # return the inverse i
  
}
