#Tabela verdade da porta OR
portaOr <- as.data.frame(
  matrix(c(0,0,0,0,1,1,1,0,1,1,1,1)  
         ,ncol = 3, nrow=4, byrow = T))
colnames(portaOr) <- c('Entrada1', 'Entrada2', 'Saida')

#Limiar de ativa��o
limiarOr <- 0.5

#Fun��o pra ativar ou n�o o neur�nio
ativarSaida <- function(Entrada1, Entrada2, Limiar)
{
  ifelse( Entrada1+Entrada2 > Limiar , '1', '0' )
}

#Chamadas dos neur�nios
ativarSaida(portaOr[1,1], portaOr[1,2], limiarOr)
ativarSaida(portaOr[2,1], portaOr[2,2], limiarOr)
ativarSaida(portaOr[3,1], portaOr[3,2], limiarOr)
ativarSaida(portaOr[4,1], portaOr[4,2], limiarOr)
