setwd('C:\\Users\\logonrmlocal\\Downloads\\BreastCancerWisconsin3')
dataset <- 'breast-cancer-wisconsin.data'
nome <- 'breast-cancer-wisconsin.names'
dados <- read.csv(dataset, header = F, sep=',')
nomes <- read.csv(nome, header = F, sep=',')
head(dados)
colnames(dados) <- t(nomes)
set.seed(123) 


#install.packages('neuralnet')
library(neuralnet)
View(dados)
dados

#Criação das variáveis "binárias" das espécies
dados$Benigno <- dados$Class == 2
dados$Maligno <- dados$Class == 4

#Segmentação de dados no fator 80/20
exemplo <- sample(1:nrow(dados), nrow(dados)*0.8)
treino <- dados[exemplo,] #80% dos dados
teste <- dados[-exemplo,] #20% dos dados


#Criação da Rede Neural que explica as espécies a partir das características
#usando a base de treino e com 2 camadas intermediárias, com 2 e 2 neurônios respectivamente
redeNeural <- neuralnet(Benigno+Maligno ~ ClumpThickness+UniformityOfCellSize+UniformityOfCellShape+MarginalAdhesion+SingleEpithelialCellSize+BlandChromatin+NormalNucleoli+Mitoses
                        ,treino, hidden = c(5,5,3)) 

#Impressão da Rede 
plot(redeNeural)



##Apresentando só alguns elementos da Rede
##########################################
#Neuronios de entrada
plot(redeNeural,
     col.entry='orange',
     col.entry.synapse = 'orange',
     col.out='gray',
     col.out.synapse = 'gray',
     col.hidden='gray',
     col.hidden.synapse = 'gray',
     col.intercept='gray',
     show.weights=F,
     information=F)

#Neuronios camada escondida
plot(redeNeural,
     col.entry='gray',
     col.entry.synapse = 'gray',
     col.out='gray',
     col.out.synapse = 'gray',
     col.hidden='orange',
     col.hidden.synapse = 'gray',
     col.intercept='gray',
     show.weights=F,
     information=F )

#Sinapses
plot(redeNeural,
     col.entry='gray',
     col.entry.synapse = 'gray',
     col.out='gray',
     col.out.synapse = 'gray',
     col.hidden='gray',
     col.hidden.synapse = 'orange',
     col.intercept='gray',
     show.weights=F,
     information=F )

#Neurônios de Saída
plot(redeNeural,
     col.entry='gray',
     col.entry.synapse = 'gray',
     col.out='orange',
     col.out.synapse = 'orange',
     col.hidden='gray',
     col.hidden.synapse = 'gray',
     col.intercept='gray',
     show.weights=F,
     information=F )

#Bias
plot(redeNeural,
     col.entry='gray',
     col.entry.synapse = 'gray',
     col.out='gray',
     col.out.synapse = 'gray',
     col.hidden='gray',
     col.hidden.synapse = 'gray',
     col.intercept='orange',
     show.weights=F,
     information=F )

#Valor das sinapses
plot(redeNeural,
     col.entry='gray',
     col.entry.synapse = 'gray',
     col.out='gray',
     col.out.synapse = 'gray',
     col.hidden='gray',
     col.hidden.synapse = 'orange',
     col.intercept='orange',
     show.weights=T,
     information=F )

#Rede Colorida
plot(redeNeural,
     col.entry='orange',
     col.entry.synapse = 'orange',
     col.out='purple',
     col.out.synapse = 'purple',
     col.hidden='darkgreen',
     col.hidden.synapse = 'darkgreen',
     col.intercept='navy',
     show.weights=F,
     information= T)


#Detalhes da Rede
redeNeural$weights[[1]][[1]]
redeNeural$weights[[1]][[2]]


#Todos os pesos, origens e destino dos valores
redeNeural$result.matrix

#Faz a predição utilizando a Rede criada e os dados de teste
validacao <- compute(redeNeural,
                     teste[,c("ClumpThickness","UniformityOfCellSize", "UniformityOfCellShape", 
                              "MarginalAdhesion","SingleEpithelialCellSize", "BlandChromatin", 
                              "NormalNucleoli", "Mitoses")])

#Transforma o cálculo das saídas em um Data Frame
validacao <- as.data.frame(validacao$net.result)

#Renomeia as saídas de acordo com as espécies
colnames(validacao) <- c('Benigno','Maligno')

#Cria uma nova Variável no Data Frame com o maior resultado encontrado
validacao$resultado <- colnames(validacao[,1:2])[max.col(validacao[,1:2], ties.method = 'first')]

#Criação da Matriz de Confusão, cruzando a Validação e Teste
matrizConfusao <- table(validacao$resultado,teste$Class)
sum(diag(matrizConfusao)*100 / sum(matrizConfusao))

#Impressão da Matriz
matrizConfusao


tp <- matrizConfusao[1,1]
fp <- matrizConfusao[1,2]
tn <- matrizConfusao[2,1]
fn <- matrizConfusao[2,2]

acuracia <- (tp + tn) / (tp + tn + fp + fn)
precisao <- tp / (tp +fp)
recall <- tp / (tp + fn)
f1score <- (2* tp)/(2*tp+fp+fn)

acuracia
precisao
recall
f1score

dados$BareNuclei <- NULL
cor(dados)
