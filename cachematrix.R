## The program saves time when calculating the inverse of a matrix by caching the inverse values of a matrix

## The function below stores a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Create the inverse variable
    cache <- NULL
    
    ## Function sets the matrix
    setMatrix <- function(value) {
        x <<- value
        cache <<- NULL
    }
    
    ## Function gets the matrix
    getMatrix <- function() {
        x
    }
    
    ## Function sets the inverse of the matrix
    cacheInverse <- function(calc) {
        cache <<- calc
    }
    
    ## Function gets the inverse of the matrix
    getInverse <- function() {
        cache
    }
    
    ## Returns a list of the above functions
    list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

## The function below calculates the inverse of the special matrix returned by the makeCacheMatrix function
cacheSolve <- function(y, ...) {
    ## Returns a matrix that is the inverse of the passed argument
    inverse <- y$getInverse()
    
    ## Returns the inverse if it is already set
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## Gets the matrix
    data <- y$getMatrix()
    
    ## Calculates the inverse
    inverse <- solve(data)
    
    ## Set the invese to the object
    y$cacheInverse(inverse)
    
    ## Returns the matrix
    inverse
}