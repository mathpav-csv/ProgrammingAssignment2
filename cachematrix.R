## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<-y
    inv <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inverse){inv <<- inverse}
  getInverse <- function(){inv}
  list(set = set, # gives the name 'set' to the set() function defined above
       get = get, # gives the name 'get' to the get() function defined above
       setInverse = setInverse, # gives the name 'setInverse' to the setInverse() function defined above
       getInverse= getInverse) # gives the name 'getInverse' to the getInverse() function defined above
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
   inv <- x$getInverse()        
   if(!is.null(inv)){
   message("getting cached data")
   return(inv)
   }
   mat <- x$get()
   inv <- solve(mat,...)
   x$setInverse(inv)
   inv
}

## Examples 

matrix1 <- makeCacheMatrix(matrix(1:4,nrow = 2,ncol = 2))
matrix1$get()
matrix1$getInverse()

cacheSolve(matrix1)
cacheSolve(matrix1)
