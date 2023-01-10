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


bolaTotal<-c(bola1,bola2,bola3,bola4,bola5,bola6)



moda = function(dados) {
  vetor = table(as.vector(dados))
  names(vetor)[vetor == max(vetor)]
}


moda(bolaTotal)

somaBola<-data.frame()
for (i in 1:60){
  somaBola[i,1]<-i
  somaBola[i,2]<-length(bolaTotal[bolaTotal==i])
  #count2<-count2+1
  
}
names(somaBola)<- c("Bola","SomaTotal")

porcentagem <- somaBola[order(somaBola$SomaTotal,decreasing = TRUE),]

somaBola$Porcentagem <- (somaBola$SomaTotal/length(bolaTotal))*100

porcentagem

porcentagem$ordem <- c(1:60)

plot(somaBola$Bola,somaBola$SomaTotal)
