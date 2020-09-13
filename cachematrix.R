## Primero hacemos una copia de makeVector que se nos dio

## makeVector <- function(x = numeric()) {
##        m <- NULL
##        set <- function(y) {
##                x <<- y
##                m <<- NULL
##        }
##        get <- function() x
##        setmean <- function(mean) m <<- mean
##        getmean <- function() m
##        list(set = set, get = get,
##             setmean = setmean,
##             getmean = getmean)
## }

## Para crear makeCacheMatrix ya que crea el vector que deseamos
## cambiaremos el nombre de algunas variables para que se entienda mejor, 
## pero en esencia queda igual, solamente cambiaremos “mean” Media por “matinv”
## matriz invertida

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatinv <- function(matinv) m <<- matinv
        getmatinv <- function() m
        list(set = set, get = get,
             setmatinv = setmatinv,
             getmatinv = getmatinv)
}

## Después se hace algo similar con 

## cachemean <- function(x, ...) {
##        m <- x$getmean()
##        if(!is.null(m)) {
##                message("getting cached data")
##                return(m)
##        }
##        data <- x$get()
##        m <- mean(data, ...)
##        x$setmean(m)
##        m
## }


## Para crear cacheSolve donde también cambiamos “mean” Media por “matinv” 
## matriz invertida, la clave es la sección m <- mean(data, …) donde la función
## mean se cambia con solve m <- solve(data, ...)


cacheSolve <- function(x, ...) {
        m <- x$getmatinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatinv(m)
        m
}

## Donde solve en lugar de sacar la media, como en la primera formula, calculará
## la inversa de la matriz, esta función genérica resuelve la ecuación a 
## %*% x = b para x, donde b puede ser un vector o una matriz, pero si b no se 
## define nos da la inversa de la matriz
