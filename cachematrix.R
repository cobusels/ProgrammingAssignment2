## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# creates a R object(list) that provides access to stored matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # init invers to NULL
  setm <- function(y) {
    x <<- y #set x to the newly assgned object
    inv <<- NULL #if new matrix assigned, new inverse must be calculated
  }
  getm <- function() x # return the stored matrix
  setinv <- function(inb_inv) inv <<- inb_inv #assign value in inb_inv to inv
  getinv <- function() inv # return inv value
  list(setm = setm, getm = getm,
       setinv = setinv,
       getinv = getinv) #make a list of all the functions defined for outside access

}


## Write a short comment describing this function
#This function reads the stored inverse from the object created by makeCacheMatrix 
#else it calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv() # read inv using getinv created in makeCacheMatrix
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) # if inv is not null simply retrieve and return it. End the function
  }
  data <- x$getm() #ELSE: read x from using getm created in makeCacheMatrix
  inv <- solve(data, ...) # perform the inverse
  x$setinv(inv) # set inv using setinv created in makeCacheMatrix
  inv
}
