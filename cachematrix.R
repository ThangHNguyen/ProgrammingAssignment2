## These two functions take a square invertible matrix, compute its invese and added to the cache if its inverse is not already  
## in the cache. Otherwise, it will get the matrix inverse from the cache. 


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL        
        set <- function(y) {                         #function set                  -- return y = x and m = NULL
                x <<- y
                m <<- NULL
        }
        
        get <- function() x                          #function get                  -- return x
        
        setinverse <- function(solve) m <<- solve    #function setinverse             -- calculate matrix inverse and assign it to m
        
        getinverse <- function() m                   #function getinverse              -- return m
        
        list(set = set, get = get,                   #return a list of 4 functions, set, get, setinverse, getinverse 
             setinverse = setinverse,
             getinverse = getinverse)
}

x = matrix(1:4,2)
a <- makeCacheMatrix(x)


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {                     ## Return the inverse matrix of 'x'
        
        m <- x$getinverse()                          #query the cache
        if(!is.null(m)) {                            #if x's inverse is already in the cache
                message("getting cached data")
                return(m)                            #return its value from the cache, no computation needed
        }
        data <- x$get()                              #if x's inverse is not in the cache
        m <- solve(data, ...)                        #compute x's inverse
        x$setinverse(m)                              #save it to the cache
        m                                            #display x's inverse        
}

b <- cacheSolve(a)
b
