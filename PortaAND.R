#Tabela verdade da porta AND
portaAnd <- as.data.frame(
matrix(c(0,0,0,0,1,0,1,0,0,1,1,1)  
      ,ncol = 3, nrow=4, byrow = T))
colnames(portaAnd) <- c('Entrada1', 'Entrada2', 'Saida')

#Limiar de ativa��o
limiarAnd <- 1.5

#Fun��o pra ativar ou n�o o neur�nio
ativarSaida <- function(Entrada1, Entrada2, Limiar)
{
  ifelse( Entrada1+Entrada2 > Limiar , '1', '0' )
}

#Chamadas dos neur�nios
ativarSaida(portaAnd[1,1], portaAnd[1,2], limiarAnd)
ativarSaida(portaAnd[2,1], portaAnd[2,2], limiarAnd)
ativarSaida(portaAnd[3,1], portaAnd[3,2], limiarAnd)
ativarSaida(portaAnd[4,1], portaAnd[4,2], limiarAnd)
