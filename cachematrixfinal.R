## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  
  m <- NULL
  set <- function(y) {
   ## use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean ## calculate the inverse 
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
  
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  m <- x$getmean()
  # if the inverse has already been calculated
  if(!is.null(m)) {
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(m)
  }
  # otherwise, calculates the inverse 
  data <- x$get()
  m <- mean(data, ...)
  # sets the value of the inverse in the cache via the setinv function.
  x$setmean(m)
  m
}


## to  test these functions I wrote a function test()

test = function(mat){
  ## @mat: an invertible matrix
  
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
} 
## thus ends the code 
