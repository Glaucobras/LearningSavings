ms<-read_csv("MegaSenaCSV.csv",col_names = TRUE)


count<-1
bola1<-0
for (i in ms$`bola 1`){ 
  bola1[count]<-i
  count<-count+1}


count<-1
bola2<-0
for (i in ms$`bola 2`){ 
  bola2[count]<-i
  count<-count+1}


count<-1
bola3<-0
for (i in ms$`bola 3`){ 
  bola3[count]<-i
  count<-count+1}


count<-1
bola4<-0
for (i in ms$`bola 4`){ 
  bola4[count]<-i
  count<-count+1}



count<-1
bola5<-0
for (i in ms$`bola 5`){ 
  bola5[count]<-i
  count<-count+1}


count<-1
bola6<-0
for (i in ms$`bola 6`){ 
  bola6[count]<-i
  count<-count+1}


bola1
bola2
bola3
bola4
bola5
bola6

#Vetor com todos os números sorteados em todos os concursos
bolaTotal<-c(bola1,bola2,bola3,bola4,bola5,bola6)



moda = function(dados) {
  vetor = table(as.vector(dados))
  names(vetor)[vetor == max(vetor)]
}

#Valor que mais saiu dentre os 60 números
moda(bolaTotal)

#Soma das ocorrências de todas as bolas independente da ordem e do sorteio.
somaBola<-data.frame()
for (i in 1:60){
  somaBola[i,1]<-i
  somaBola[i,2]<-length(bolaTotal[bolaTotal==i])
  #count2<-count2+1
  
}
names(somaBola)<- c("Bola","TotalOcorrencias")
somaBola

#Porcentagem de quanto saiu cada bola
somaBola$Porcentagem <- (somaBola$TotalOcorrencias/length(bolaTotal))*100
somaBola



#Dados ordenados de maneira decrescente (das bolas que mais sairam para as que menos sairam)
somaBolaOrdenado <- somaBola[order(somaBola$TotalOcorrencias,decreasing = TRUE),]
somaBolaOrdenado


#Adicionando uma coluna para ajudar a visualizar a ordem de 1 a 60 (das que mais saem para as que menos saem)
somaBolaOrdenado$ordem <- c(1:60)
somaBolaOrdenado


plot(somaBola$Bola,somaBola$TotalOcorrencias)
hist(bolaTotal,breaks = 15)

#Seguindo a teoria das probabilidades, numa situação ideal cada bola tem chance 1/60 de ser sorteada.
#Ou seja, 1,666% de ser sorteada se muitos sorteios forem realizados.
#Considerando as porcentagens do dataframe, entendemos que as bolas que ainda não atingiram essa porcentagem
#São as que tem mais tendência de serem sorteadas para que o equilíbrio seja atingido matematicamente falando.
#No caso desses dados são as bolas de número de ordem acima de 32
#São calculadas pelo comando abaixo

numerosdasorte <- somaBolaOrdenado$Bola[somaBolaOrdenado$ordem>=32]
numerosdasorte

#Número de volantes possíveis com os números da sorte é dado pela combinação abaixo
factorial(length(numerosdasorte))/(factorial(6)*factorial(length(numerosdasorte)-6))

#Criando uma nova tabela apenas com os valores das bolas sorteadas divididas por concurso.
ms2<-ms
ms2$Concurso<-NULL
ms2$Data<-NULL
ms2

#Com a função apply, vamos calcular as médias das 6 bolas de cada sorteio (concurso)
apply(ms2,1,mean)

#Criando uma nova linha na tabela ms2 com as médias calculadas
ms2$media<-apply(ms2,1,mean)
ms2

#Ordenando os dados de maneira decrescente pela média
ms2<-ms2[order(ms2$media),]
ms2

#Criando um histograma para entender a média
hist(ms2$media)

#Calculando kurtosis e Skewness
kurtosis(ms2$media)
skewness(ms2$media)

#Os valores de Kurtosis e Skewness mostram que é uma boa aproximação de uma normal
# assim podemos tomar a média como o valor amostral que mais tem chance de ocorrer.

mean(ms2$media)

#A média calculada é de 30,51. 
#Assim, os volantes a ser calculados devem seguir a regra:
#Os números que vão compor o volante devem estar entre os números calculados por numerosdasorte que são os que tem porcentagem de sorteio menor que 1,66% até o momento.
#E que a média dos 6 números aleatoriamente selecionados para cada volante seja 30,51.

#O código abaixo seleciona 100 volantes dentre todos que seguem as regras acima. 
#Para ter uma margem, colocamos uma faixa de média que vai de 30.5 a 30.52
countv<-0
while (countv < 100){
  volante<-sample(numerosdasorte,6)
  if (mean(volante)>=30.5 & mean(volante)<=30.52)
      {print(sort(volante))
       #print(mean(volante)) 
       countv<-countv+1
    
      }
  
  
  }


# Ao final temos 100 volantes potenciamente mais suceptíveis de serem sorteados
# dentre os 50 milhões de possibilidades da população total.
# Para volantes diferentes, basta rodar o loop final novamente.





