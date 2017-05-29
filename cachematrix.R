## Below are a pair of functions that cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## stores the cached value
        ## initialise to NULL
        cache<-NULL
        
        set<-function(y){
             x<<-y
             cache<<-NULL
        }
        
        # get the value of the matrix
        get<-function()x
        # invert the matrix and store in cache
        setMatrix<-function(inverse)cache<<-inverse
        # get the inverted matrix from cache
        getInverse<-function()cache
        
        # return the created functions to the working environment
        list(set=set,get=get,
             setMatrix=setMatrix,
             getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## attempt to get the inverse of the matrix stored in cache
        cache<-x$getInverse()
        
        ## return inverted matrix from cache if it exists
        ## else create the matrix in working environment
        if(!is.null(cache)){
               message("getting cached data")
          
               # display matrix in console
               return(cache)
        }
        
        # create matrix since it does not exist
        matrix <-x$get()
        
        # make sure matrix is square and invertible
        # if not, handle exception cleanly
        tryCatch({
                # set and return inverse of matrix
                cache<-solve(matrix, ...)
        },
        error=function(e){
              message("Error:")
              message(e)
              
              return(NA)
        },
        warning=function(e){
               message("Warning:")
               message(e)
               
               return(NA)
        },
        finally={
               # set inverted matrix in cache
               x$setMatrix(cache)
        })
        ## Return a matrix that is the inverse of 'x'
        return(cache)
}
