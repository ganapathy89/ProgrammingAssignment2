## "makeCacheMatrix" is used to feed the input data and "cacheSolve" 
## calculates the inverse of the matrix (if any). If "cacheSolve" is used again 
## for the same matrix, it returns a message "getting cached data" to 
## indicate the cache function. 


## The "makeCacheMatrix" is used to feed the input data. The "<<-" symbol replaces 
## y with the data of x.  

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
        
        }

    get <- function() x
    
    setmatrix <- function(solve) m <<- solve
    
    getmatrix <- function() m
    
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)



}



## the variables in "cacheSolve" get data from previous function(lexical scoping)
 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$getmatrix()      
    
    if(!is.null(m)){        ## if 'm' is not NULL, get data stored in 'm'
        
            message("getting cached data")
        
            return(m)
        
        }

    
    data <- x$get()         ## if m is NULL, calculate inverse.
    
    m <- solve(data, ...)
    
    x$setmatrix(m)

    m


  }
