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
names(somaBola)<- c("Bola","SomaTotal")
somaBola

#Porcentagem de quanto saiu cada bola
somaBola$Porcentagem <- (somaBola$SomaTotal/length(bolaTotal))*100
somaBola



#Dados ordenados de maneira decrescente (das bolas que mais sairam para as que menos sairam)
somaBolaOrdenado <- somaBola[order(somaBola$SomaTotal,decreasing = TRUE),]
somaBolaOrdenado


#Adicionando uma coluna para ajudar a ordenar de 1 a 60 (das que mais saem para as que menos saem)
somaBolaOrdenado$ordem <- c(1:60)
somaBolaOrdenado


plot(somaBola$Bola,somaBola$SomaTotal)
hist(bolaTotal,breaks = 15)

#Seguindo a teoria de probabilidades, numa situação ideal cada bola tem chance 1/60 de ser sorteada.
#Ou seja, 1,666 % de ser sorteada se muitos sorteios forem realizados.
#Considerando as porcentagens do dataframe, entendemos que as bolas que ainda não atingiram essa porcentagem
#São as que tem mais tendência de serem sorteadas, para que o equilíbrio seja atingido matematicamente falando.
#No caso desses dados são as bolas de número de ordem acima de 32
#São calculadas pelo comando abaixo

numerosdasorte <- somaBolaOrdenado$Bola[somaBolaOrdenado$ordem>=32]
numerosdasorte

#Exemplos de volantes que podem ser extraídos desses números
for (i in 1:100){
print(sample(numerosdasorte,6))
  }





