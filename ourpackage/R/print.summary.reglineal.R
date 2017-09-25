`print.summary.reglineal` <-
function(x, ...){
cat("Call:\n")
print(x$call)
cat("\n")

printCoefmat(x$coefficients, P.value=TRUE, has.Pvalue=T) #para pintar la tabla con la significación
}

