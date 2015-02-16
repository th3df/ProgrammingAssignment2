## Put comments here that give an overall description of what your
## functions do

## Function creates a special matrix object that stores a matrix, its inverse matrix,and 
## four methods/functions for getting access to the matrices
## The four functionsare getmtx, setmtx, getinvmtx, and setinvmtx. 

makeCacheMatrix <- function(mtx = matrix()) {
    
    cached_inv_mtx<-NULL        # erase any cached inverse matrix
    
    setmtx<-function(new_mtx){  # function takes in a new_mtx
        mtx<<-new_mtx           # copies new_matx into parent scope mtx
        cached_inv_mtx<<-NULL   # erases any cached inv_mtx that may exist
    }
    
    getmtx<-function() mtx      # function returns mtx in this parent scope
    
    setinvmtx <- function(inv_mtx) cached_inv_mtx <<-inv_mtx
    
    getinvmtx <- function() cached_inv_mtx # Returns the cached Inverse matrix, if any
    
    list(setmtx=setmtx, 
         getmtx=getmtx, 
         setinvmtx=setinvmtx, 
         getinvmtx=getinvmtx)
}


## The function takes a matrix object as a parameter and uses the object methods/functions
## to get access to the inverse matrix and return it if it exists. Otherwise the function
## will calculate the inverse matrix and return it.

cacheSolve <- function(mtx_obj, ...) {
    inv_mtx <- mtx_obj$getinvmtx()     # Get inverse matrix from mtx_obj
    
    if (!is.null(inv_mtx)) {           # if inverse matrix already exists, 
        message("getting cached inverse matrix")       # print a message as such
        return(inv_mtx)                # return the cached inverse matrix
    }
    
    mtx <- mtx_obj$getmtx()            # get the matrix from the object
    inv_mtx <- solve(mtx, ...)         # calculate the inverse matrix
    mtx_obj$setinvmtx(inv_mtx)         # insert the inverse matrix into mtx_obj
    
    inv_mtx                            # return the inverse matrix to the calling env
}

