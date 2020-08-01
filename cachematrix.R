## set() assigns a matrix
## get() returns the same
## set_inv() assigns the inverse
## get_inv() returns the same
## list gets the data in char

## the following func block stores the cached information of the matrix
## changes can be made into it by get,set,set_inv,get_inv

makeCacheMatrix <- function(x = matrix()) {
  inv_mat<-NULL
  set<-function(y){
    x<<-y
    inv_mat<<-NULL
  }
  get <- function() x
  set_inv <- function(inv) inv_mat <<- inv
  get_inv <- function() inv_mat
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)

}


## the following func block retireves the cached inverse of matrix 
## if NULL returns then it computes the inverse and set_inv() in the above func

cacheSolve <- function(x, ...) {
  inverse_mat <- x$get_inv()
  if(!is.null(inverse_mat)){
    message('Hold on!, getting the cached data in light speed')
    return(inverse_mat)
  }
  matr_x <- x$get()
  inverse_mat <- solve(matr_x,...)
  x$set_inv(inverse_mat)
  inverse_mat
        ## inverse matrix
}
