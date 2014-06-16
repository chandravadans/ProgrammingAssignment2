## Makes the special matrix that will cache the inverse
makeCacheMatrix <- function(x = matrix()) {
    
    cachedInverse <- NULL
    
    # Setter for the data and resets the cached Inverse. 
    setData <- function(mat){
        x <<- mat
        cachedInverse <<- NULL
    }
    
    #Returns the matrix whose inverse is cached presently
    getData <- function(){
        x
    }
    
    #Setter function for the cached inverse matrix. 
    setInverse <- function(newInverse){
        cachedInverse <<- newInverse
    }
    
    #Returns the cached inverse matrix
    getInverse <- function(){
        cachedInverse
    }
    
    #Returns a list of all the above functions, along with the cached inverse
    list(getData = getData,
         setData = setData,
         getInverse = getInverse,
         setInverse = setInverse
         )
}


## Caches the inverse of a matrix, eliminating need for repeated computation.
## Checks if a cached inverse already exists. If not, computes and caches the inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    #Check if the inverse is already cached
    inverse <- x$getInverse()
    if(!is.null(inverse)){
        
        #The inverse has been cached. Just return it
        print("cached inverse")
        return (inverse)
    }
    else{
        #Otherwise, calculate the inverse and update the cached inverse to it
        matr <- x$getData()
        inverse <- solve(matr, ...)
        x$setInverse(inverse)
        inverse
    }
}