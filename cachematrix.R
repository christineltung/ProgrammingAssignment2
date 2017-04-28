## Overall description: makeCacheMatrix and cacheSolve are functions that use the output of makeCacheMatrix 
## to calculate the inverse matrix, or recall the cached inverse matrix

## More specifically, makeCacheMatrix is a function that creates a list of 4 functions that
## sets, recalls, sets the inverse, and then recalls the inverse matrix in addition to the original matrix

makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL  # Cached inverse matrix, initially NULL
    set <- function(y) {
        x <<- y
        mat <<- NULL  # Reset the cache to null because the data has changed
    }
    get <- function() x
    set_inv <- function(new_inv) {
        # Set the cache to the given value
        mat <<- new_inv
    }
    get_inv <- function() {
        # Return the cached value of the inverse
        mat
    }
    cached_matrix <- list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
    return(cached_matrix)
}


## cacheSolve is a function using the output returned from 'makeCacheMatrix'
## It returns the inverse matrix if there is one cached
## If there is no cached value, the inverse matrix is calculated using solve()
## The inverse matrix is then set and cached using the function cached_matrix$set_inv, and
## the function returns the inverse matrix

cacheSolve <- function(cached_matrix, ...) {
    cached_inv <- cached_matrix$get_inv() # Get current cached inverse
    if(!is.null(cached_inv)) {
        # Return the cached value of the inverse if the matrix is not null
        return(cached_inv)
    }
    # No cached value. 
    # Compute the inverse based on the underlying data
    data = cached_matrix$get()
    inv = solve(data)
    cached_matrix$set_inv(inv)  # Save the calculated inverse to the cache
    return(inv)
    
}