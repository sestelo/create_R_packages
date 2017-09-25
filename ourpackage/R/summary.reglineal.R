`summary.reglineal` <-
function(x, ...){

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

