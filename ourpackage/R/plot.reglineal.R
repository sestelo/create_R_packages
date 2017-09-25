`plot.reglineal` <-
function(x, ...){
data <- x$data
coef <- coef(x)
lab <- colnames(data)
plot(data[, 3], data[, 1], xlab = lab[3], ylab = lab[1], ...)
abline(coef[1], coef[2], lwd = 2)
}

