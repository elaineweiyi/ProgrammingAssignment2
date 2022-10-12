## cache matrix and get the inverse matrix from cache

## creates a special "matrix" object that can cache its inverse.It doesn't return but chache

makeCacheMatrix <- function(x = matrix(),...) {
  inverse_mtx<-NULL  #pass null to the inverse 
  setmatrix<-function(y){
    x<<-y           #set(y)，x=y,inverse_mtx=null
    inverse_mtx<<-NULL
  }
  getmatrix<-function()x  #pass x to getmatrix
  set_inverse_matrix<-function(solve) inverse_mtx<<-solve
  get_inverse_matrix<-function()inverse_mtx
  list(setmatrix=setmatrix,getmatrix=getmatrix,set_inverse_matrix=set_inverse_matrix,get_inverse_matrix=get_inverse_matrix)
}


## the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_mtx<-x$get_inverse_matrix() #use the function above get_inverse_matrix,to see if is not null
  if (!is.null(inverse_mtx)){
    message("getting cached data")
    return(inverse_mtx)
  }# call cache value
  data<-x$getmatrix()  #the original matrix
  inverse_mtx<-solve(data,...)
  x$set_inverse_matrix(inverse_mtx) #cache inverse matrix
  inverse_mtx
  }
