`reglineal.default` <-
function(x, y, ...){

x <- as.matrix(x)
y <- as.vector(y)
res <- reglin(x, y)
res$data <- cbind(y, x) #guardamos los datos para el plot
res$call <- match.call() #guarda la llamada a la función
class(res) <- "reglineal" #definimos la clase
return(res)
}

