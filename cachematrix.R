##BretIG 
##rprog-004, 18June2014
##Programming Assignment 2
##makeCacheMatrix creates a list of values from the predefined matrix
##cacheSolve checkes to see if the matrix has already been inverted...
##and retunrs that value, else it computes the inverse

## create a matrix to test
m <- makeCacheMatrix()
m$set(matrix( c(0,2,2,0),2,2))
m$get()
cacheSolve(m)


makeCacheMatrix <- function (x=matrix()) {
        m <- NULL ##ensures the matrix is empty
        
        #initialize matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #returns original matrix
        get <- function() x
        
        #Inverts the matrix using solve
        setInverse <- function(){
                m <<- solve(x)
                m
        }
        
        ##returns inverse of matrix
        getInverse <- function () m
        
        ## return makeCacheMatrix object
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


cacheSolve <- function(x, ...) {
        ##define m
        m <- x$getInverse()
        ## if m is empty in the function, it will get m from cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## otherwise it will return the inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m                  
}
