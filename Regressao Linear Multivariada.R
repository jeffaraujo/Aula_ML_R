#Bivariada
modelo <- lm(mpg~disp, data=mtcars)
cor(mtcars[,c("mpg",'disp')])

View(data.frame(cor(mtcars)))

modelo$coefficients
modelo$coefficients[1]
modelo$coefficients[2]

summary(modelo)$r.squared
summary(modelo)$adj.r.squared

#Multivariada

plot(x=mtcars$disp, y=mtcars$mpg)
abline(modelo)

predict(modelo, data.frame(disp=200))

modelo <- lm(mpg ~ disp+hp+cyl, data=mtcars)

summary(modelo)$r.squared
summary(modelo)$adj.r.squared

#Predição para novos dados
predict(modelo, data.frame(disp=200, hp=100, cyl=4))