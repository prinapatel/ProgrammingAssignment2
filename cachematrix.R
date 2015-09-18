#this pair of function will cache the inverse of a matrix rather than compute it repeatadly
#makeCacheMatrix creates a matric object
#cacheSolve computes the inverse of the matrix object created by makeCacheMatrix - if the inverse has already been calculated then cacheSolve will retrieve the inverse from cache

## Write a short comment describing this function
#<<- operator causes a search to be made through parent environments for an existing definition of the variable being assigned.
#set is a function that defines new matrix y to be set to x
#get is a function that returns x
#setinverse sets the inverse, m to inverse
#getinverse returns in the inverse m]
#list is the special vector that contains all the defined functions

#this function makeCacheMatrix creates an object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setinverse<-function(inverse) m<<-inverse
    getinverse<-function() m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

#this function cacheSolve computes the inverse of the special matrix object made by makeCacheMatrix, if it has already been computed it's retrieved from cache rather than recomputed
#solve here is the function that computes the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()   #does the inverse already exisit?
        if(!is.null(m)){return(m)}
        d<-x$get()
        m<-solve(d)
        x$setinverse(m)
        m
}
