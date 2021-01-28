## thanks for evaluating, wishing you health in the ongoing pandemic

## the function is basically unchanged in comparison to the given one
## for the sake of clarity I changed the identifiers from m/mean to inv/sol 
## but the changes that have to be made are just two: 
## 1. the data has to be a matrix now, so x = matrix() instead of x = mean()
## 2. one doesn't calculate the mean but the inverse, so inv = solve() instead of m = mean()

makeCacheMatrix = function(x = matrix()) {
    inv = NULL
    set = function(y) {
        x <<- y
        inv <<- NULL
    }
    get = function() x
    setinv = function(sol) inv <<- sol
    getinv = function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve = function(x, ...) {
    inv = x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data = x$get()
    inv = solve(data, ...)
    x$setinv(inv)
    inv
}

## Example (run together with the above)
a = matrix(c(1,2,3,4),nrow=2,ncol=2)
b = makeCacheMatrix(a)
cacheSolve(b) ## saves the inverse because it isn't already
cacheSolve(b) ## as one can see it gets the cached inverse now instead of calculating it again