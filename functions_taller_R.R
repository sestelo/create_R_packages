setwd("~/Desktop/Taller_R")

datos <- read.table("production.txt", header = TRUE)


# Función Regresión lineal
#   Input: 
#        x: variable independiente
#        y: variable respuesta



#Creamos una función para regresión lineal
reglin <- function(x, y){
	
	#calculamos beta   (X'X)^(-1) X'Y
	beta <- (solve(t(x) %*% x)) %*% t(x) %*% y
	coef <- beta[, 1]
	
	#valores ajustados
	fit <- as.vector(x %*% beta)
	
	#obtenemos los residuos
	res <- y - fit
	n <- nrow(x); p <- ncol(x); df <- n - p
	var_err <- sum(res**2)/df
	
	#calculamos matriz covarianzas
	vcov <- var_err*solve(t(x) %*% x)
		
	#resultados
	list(coefficients = coef, fitted.values = fit, residuals = res, var_err = var_err, vcov = vcov, df = df)
}





#Creamos la función genérica "reglineal"

reglineal <- function(x, ...) UseMethod("reglineal")





# Método por defecto de "reglineal"

reglineal.default <- function(x, y, ...){
	
	x <- as.matrix(x)
	y <- as.vector(y)
	res <- reglin(x, y)
	res$data <- cbind(y, x) #guardamos los datos para el plot
	res$call <- match.call() #guarda la llamada a la función
	class(res) <- "reglineal" #definimos la clase
	return(res)
}






# Creamos un nuevo método de la función print

print.reglineal <- function(x, ...){
cat("Call:\n") 
print(x$call) 
cat("\nCoefficients:\n") 
print(x$coefficients)	
}





# Nuevo método para la función summary

summary.reglineal <- function(x, ...){	

	#inferencia coef
	et <- sqrt(diag(x$vcov))
	tvalues <- abs(as.vector(x$coefficients))/et
	pvalues <- 2*pt(tvalues, x$df, lower.tail = FALSE)

	tabla <- cbind(Estimate = coef(x),
					StdErr = et,
					t.values = tvalues,
					p.values = pvalues)
				
	res <- list(call = x$call, coefficients = tabla)
	class(res) <- "summary.reglineal"
	return(res)	
}






# Método print.summary

print.summary.reglineal <- function(x, ...){
	cat("Call:\n")
	print(x$call)
	cat("\n")
	
	printCoefmat(x$coefficients, P.value=TRUE, has.Pvalue=T) #para pintar la tabla con la significación
}





# Nuevo método para la función genérica "reglineal" 

# (Vamos a crear 2 métodos para la función "reglineal", uno premite el cálculo introduciendo las variables directamente --reglineal.default(x,y,...)-- mientras que el otro método permite el uso de la fórmula --reglineal.formula(formula,data=list(),...)-- )

reglineal.formula <- function(formula, data = list(), ...){
	mf <- model.frame(formula = formula, data = data)
	mt <- attr(mf, "terms")
	x <- model.matrix(mt, data = mf)
	y <- model.response(mf)
	
	res <- reglineal.default(x, y, ...)
	res$call <- match.call()
	res$formula <- formula
	return(res)
}




# Nuevo método para la función plot

plot.reglineal <- function(x, ...){
	data <- x$data	
	coef <- coef(x)
	lab <- colnames(data)
	plot(data[, 3], data[, 1], xlab = lab[3], ylab = lab[1], ...)
	abline(coef[1], coef[2], lwd = 2)
}




# Construcción del paquete

mylist <- c("reglin", "reglineal", "reglineal.default", "print.reglineal", "summary.reglineal", "print.summary.reglineal", "reglineal.formula", "plot.reglineal")

package.skeleton(name = "ourpackage", mylist)





