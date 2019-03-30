#Configuranco o ambiente
setwd('C:\\Users\\logonrmlocal\\Downloads\\AirQualityUCI')
dados <- read.csv('AirQualityUCI.csv', header = T, sep = ';')

#Alterando o tipo de dado
dados$Date <- as.Date(dados$Date, format("%d/%m/%Y"))
dados$CO.GT. <- as.numeric(as.character(gsub(x=dados$CO.GT., ',', '.')) )
dados$C6H6.GT. <- as.numeric(as.character(gsub(x=dados$C6H6.GT., ',', '.')) )
dados$T <- as.numeric(as.character(gsub(x=dados$T, ',', '.')) )
dados$RH <- as.numeric(as.character(gsub(x=dados$RH, ',', '.')) )
dados$AH <- as.numeric(as.character(gsub(x=dados$AH, ',', '.')) )

#Limpeza dos dados para criar o modelo
dados$PT08.S1.CO. <- ifelse( is.na(dados$PT08.S1.CO.)==T, -200,dados$PT08.S1.CO.  )
dados$CO.GT. <- ifelse( is.na(dados$CO.GT.)==T, -200,dados$CO.GT.  )
dados$NMHC.GT. <- ifelse( is.na(dados$NMHC.GT.)==T, -200,dados$NMHC.GT.)
dados$C6H6.GT. <- ifelse( is.na(dados$C6H6.GT.)==T, -200,dados$C6H6.GT.)
dados$PT08.S2.NMHC. <- ifelse( is.na(dados$PT08.S2.NMHC.)==T, -200,dados$PT08.S2.NMHC.)
dados$NOx.GT. <- ifelse( is.na(dados$NOx.GT.)==T, -200,dados$NOx.GT.)
dados$PT08.S3.NOx. <- ifelse( is.na(dados$PT08.S3.NOx.)==T, -200,dados$PT08.S3.NOx.)
dados$NO2.GT. <- ifelse( is.na(dados$NO2.GT.)==T, -200,dados$NO2.GT.)
dados$PT08.S4.NO2. <- ifelse( is.na(dados$PT08.S4.NO2.)==T, -200,dados$PT08.S4.NO2.)
dados$PT08.S5.O3. <- ifelse( is.na(dados$PT08.S5.O3.)==T, -200,dados$PT08.S5.O3.) 

novoDataset <- dados[(dados$PT08.S1.CO.  > -200 & dados$CO.GT. > -200 &
dados$NMHC.GT. > -200 & dados$C6H6.GT. > -200 &
dados$PT08.S2.NMHC > -200 & dados$NOx.GT. > -200 &
dados$PT08.S3.NOx. > -200 & dados$NO2.GT. > -200 &
dados$PT08.S4.NO2. > -200 & dados$PT08.S5.O3.  > -200 ), 
c('CO.GT.','PT08.S1.CO.','NMHC.GT.','C6H6.GT.','PT08.S2.NMHC.'
  ,'NOx.GT.','PT08.S3.NOx.','NO2.GT.','PT08.S4.NO2.','PT08.S5.O3.')]

#Capturando os nomes das colunas
colnames(novoDataset)
#Realizando o cálculo (Regressão Linear)
modelo <- lm(PT08.S4.NO2. ~ CO.GT.+PT08.S1.CO.+C6H6.GT.+PT08.S2.NMHC.+NOx.GT.+PT08.S3.NOx.+NO2.GT.+PT08.S5.O3., data=novoDataset)
#Capturando o valor (Acurácia)
summary(modelo)$r.squared

#Predição para novos dados
predict(modelo, data.frame(CO.GT.=2.7, 
                           PT08.S1.CO.=1000, 
                           C6H6.GT.=12.0,
                           PT08.S2.NMHC.=2046, 
                           NOx.GT.=166, 
                           PT08.S3.NOx.=1056, 
                           NO2.GT.=165,
                           PT08.S5.O3.=1300 ))
plot(modelo)





#### Resultados
# Tentativa 1
#modelo <- lm(PT08.S4.NO2. ~ ., data=novoDataset)
#summary(modelo)$r.squared

#Predição para novos dados
#predict(modelo, data.frame(CO.GT.=2.5, PT08.S1.CO.=1350, NMHC.GT.=150, C6H6.GT.=12.0,
#                           PT08.S2.NMHC.=1046, NOx.GT.=166, PT08.S3.NOx.=1056, NO2.GT.=165,
#                           PT08.S4.NO2.=1692, PT08.S5.O3.=1268 ))

#[1] 0.964803
#1 
#1531.981 




