`reglin` <-
function(x, y){

#calculamos beta   (X'X)^(-1) X'Y
beta <- (solve(t(x) %*% x)) %*% t(x) %*% y
coef <- beta[, 1]

#valores ajustados
fit <- as.vector(x %*% beta)

#obtenemos los residuos
res <- y - fit
n <- nrow(x); p <- ncol(x); df <- n - p
var_err <- sum(res**2)/df

#calculomos matriz covarianzas
vcov <- var_err*solve(t(x) %*% x)

#resultados
list(coefficients = coef, fitted.values = fit, residuals = res, var_err = var_err, vcov = vcov, df = df)
}

