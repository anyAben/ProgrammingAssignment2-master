## This function help us cache the inverse of a matrix.
## If cache already stored the inverse of this matrix before, then just retrive the value.
## Otherwise we calcaulte it and store the vaule in cache.


## makeCacheMatrix takes a matrix, setting and getting the inverse for them.

makeCacheMatrix<-function(x=matrix()) {
    i<-NULL
    set<-function(y){
        x<<-y
        i<<-NULL
    }
    get<-function(){
        x
    }
    setInverse<-function(inverse){
        i<<-inverse
    }
    getInverse<-function(){
        i
    }
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## cacheSolve takes the inverse of matrix created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    i<-x$getInverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data<-x$get()
    i<-solve(data,...)
    x$setInverse(i)
    i
}
