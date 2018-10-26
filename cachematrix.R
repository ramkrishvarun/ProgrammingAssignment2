# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL
  setMatrix <- function(mat) {
    x <<- mat
    inversematrix <<- NULL
  }
  getMatrix <- function() x
  setinversematrix <- function(inv) inversematrix <<- inv
  getinversematrix <- function() inversematrix
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


# This function will return inverse of matrix by checking if inverse has already been computed
# If inverse is already available it skips computation

# This function assumes that the matrix is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversematrix <- x$getinverse()
  if (!is.null(inversematrix)) {
    message("getting inverse from cache")
    return(inversematrix)
  }
  y <- x$getMatrix()
  i <- solve(y, ...)
  x$setinverse(i)
  i
}

##testing the functions
example.matrix <- matrix(c(5:8),2,2)

test.function <- makeCacheMatrix(example.matrix)
cacheSolve(test.function) #Goes to computation because cache is not available

cacheSolve(test.function) #returns inverse from cache

test.function2 <- makeCacheMatrix(-example.matrix)
cacheSolve(test.function)
cacheSolve(test.function2)
