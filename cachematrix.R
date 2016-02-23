## Put comments here that give an overall description of what your functions do
##
## The first function, makeCacheMatrix creates a special "matrix", which is 
## really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverted matrix
## 4.get the value of the inverted matrix
##
## The second function, cacheSolve, calculates the inverse of the special
## "matrix" created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the
## data and sets the value of the inverse in the cache via the 'setinv' function.



## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                     # initialize inverse to null
        set <- function(y) {            
                x <<- y                 # cache matrix
                inv <<- NULL            # whenever we set matrix we init inv
        }
        get <- function() x             # get matrix 
        setinv <- function(invMATRIX) inv <<- invMATRIX  # cache inverse
        getinv <-function() inv          #get inverse
        
        # return list of functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
        m<-x$getinv()
        if(!is.null((m))){              # if inverse is present then cache it
                message("getting cached data")
                return(m)
        }
        data<-x$get()                   # get matrix from object
        inv<-solve(data,...)            # compute inverse
        x$setinv(inv)                   # set inverse in object
        inv                             # Return a matrix as the inverse of 'x'
}

