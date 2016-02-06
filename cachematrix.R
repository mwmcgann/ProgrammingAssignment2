## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Create a matrix and initialize the cache system

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
## returns original matrix
    get<-function()x
## save matrix inverse in cache
    setinv<-function(y)inv<<-y
## get cache control or inverse
    getinv<-function()inv
    list(set=set,get=get,setinv=setinv,getinv=getinv)
    

}


## Write a short comment describing this function
## returns inverse of matrix x, calculates inverse if it
## has not already been done
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data,...)
    x$setinv(inv)
    inv
}

