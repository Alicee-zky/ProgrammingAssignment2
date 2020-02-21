##assigment2##

#Matrix inversion is usually a costly computation
#there may be some benefit to caching the inverse of a matrix
#write a pair of functions that cache the inverse of a matrix

#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  #cache the inverse of matrix
  inv <- NULL
  
  #set/get for matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  #set/get for the inverse of matrix
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  #return
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
?solve
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  #return inverse if the inverse has already been calculated
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  #compute inverse of matrix
  data <- x$get()
  inv <- solve(data,...)
  
  #return
  x$setinverse(inv)
  inv
}

####test####
the_matrix <- makeCacheMatrix(matrix(4:1,2,2))
the_matrix$get()
the_matrix$getinverse()

cacheSolve(the_matrix)
the_matrix$getinverse()
cacheSolve(the_matrix)







