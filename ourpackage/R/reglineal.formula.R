`reglineal.formula` <-
function(formula, data = list(), ...){
mf <- model.frame(formula = formula, data = data)
mt <- attr(mf, "terms")
x <- model.matrix(mt, data = mf)
y <- model.response(mf)

res <- reglineal.default(x, y, ...)
res$call <- match.call()
res$formula <- formula
return(res)
}

