# Alta correlação
x <- c(1:20)
y <- x

m <- as.data.frame(cbind(x,y))

plot(m, pch=21, bg="orange", col="purple")
regressao <- lm(m)
abline(regressao, col="blue", lwd=3)
summary(regressao)$r.squared


# Baixa correlação
x <- as.integer(abs(rnorm(30,5,2)))
y <- as.integer(abs(rnorm(30,5,2)))

m <- as.data.frame(cbind(x,y))

plot(m, pch=21, bg="orange", col="purple")
regressao <- lm(m)
abline(regressao, col="blue", lwd=3)
summary(regressao)$r.squared