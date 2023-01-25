library("readr")


df<- read_csv('energy_demand_hourly_brazil.csv')
View(df)
names(df)<-c('aaa','Consumo MW')


library('tidyr')

df<-separate(df, "aaa", c("data", "hora"), sep = " ")
df<-separate(df, "data", c("Ano", "Mes", "Dia"), sep = "-")
df<-separate(df, "hora", c("H", "Minuto", "Segundo"), sep = ":")
View(df2)


head(df)
df$Segundo<-NULL
df$Minuto<-NULL

#Médias de consumo diário no natal de todos os anos

dfNatal<-df[df$Mes==12,]
dfNatal<-dfNatal[dfNatal$Dia==25,]
dfNatal$Mes<-NULL
dfNatal$Dia<-NULL
View(dfNatal)


count1<-1
dfNatalMedia<-data.frame()
for (i in unique(dfNatal$Ano)){
  aaa<-as.data.frame(dfNatal[dfNatal$Ano==i,3])
  bbb<-mean(aaa[,1])
  bbb<-round(bbb,2)
  dfNatalMedia[count1,1]<-i
  dfNatalMedia[count1,2]<-bbb
  count1<-count1+1
  print(paste(i,bbb))
  
}
names(dfNatalMedia)<-c("Ano","Media")
plot(dfNatalMedia$Ano,dfNatalMedia$Media)
         
#Médias de consumo diário no reveillon de todos os anos      
         
dfRev<-df[df$Mes==12,]
dfRev<-dfRev[dfRev$Dia==31,]
dfRev$Mes<-NULL
dfRev$Dia<-NULL
View(dfRev)


count1<-1
dfRevMedia<-data.frame()
for (i in unique(dfRev$Ano)){
  aaa<-as.data.frame(dfRev[dfRev$Ano==i,3])
  bbb<-mean(aaa[,1])
  bbb<-round(bbb,2)
  dfRevMedia[count1,1]<-i
  dfRevMedia[count1,2]<-bbb
  count1<-count1+1
  print(paste(i,bbb))
  
}                 

names(dfRevMedia)<-c("Ano","Media")
plot(dfRevMedia$Ano,dfRevMedia$Media)



# Média de consumo por ano

count1<-1
dfAnoMedia<-data.frame()
for (i in unique(df$Ano)){
  aaa<-as.data.frame(df[df$Ano==i,5])
  bbb<-mean(aaa[,1])
  bbb<-round(bbb,2)
  dfAnoMedia[count1,1]<-i
  dfAnoMedia[count1,2]<-bbb
  count1<-count1+1
  print(paste(i,bbb))
  
} 



#comparando tudo
dfAnoMedia<- dfAnoMedia[-24,]
names(dfAnoMedia)<-c("Ano","Media_Anual")
names(dfNatalMedia)<-c("Ano_Natal","Media_Natal")
names(dfRevMedia)<-c("Ano_Rev","Media_Rev")


dfTotal<- cbind(dfAnoMedia,dfNatalMedia,dfRevMedia)
dfTotal
dfTotal$Ano_Natal<-NULL
dfTotal$Ano_Rev<-NULL
dfTotal



plot(dfTotal$Ano, dfTotal$Media_Anual, col='red', pch=19)
points(dfTotal$Ano, dfTotal$Media_Natal, col='blue', pch=19)
points(dfTotal$Ano, dfTotal$Media_Rev, col='Green', pch=19)


#Verificando as diferenças entre a média anual e as medias natal e reveillon

dfTotal$AnoparaNatal<-dfTotal$Media_Anual - dfTotal$Media_Natal
dfTotal$AnoparaRev<-dfTotal$Media_Anual - dfTotal$Media_Rev
dfTotal

plot(dfTotal$Ano, dfTotal$AnoparaNatal, col='red', pch=19)
plot(dfTotal$Ano, dfTotal$AnoparaRev, col='blue', pch=19)

#Transformando os dados para GW

dfTotalGW<-dfTotal
dfTotalGW$Media_Anual<-dfTotalGW$Media_Anual/1000
dfTotalGW$Media_Natal<-dfTotalGW$Media_Natal/1000
dfTotalGW$Media_Rev<-dfTotalGW$Media_Rev/1000
dfTotalGW$AnoparaNatal<-dfTotalGW$AnoparaNatal/1000
dfTotalGW$AnoparaRev<-dfTotalGW$AnoparaRev/1000
dfTotalGW


plot(dfTotalGW$Ano, dfTotalGW$AnoparaNatal, col='red', pch=19)
plot(dfTotalGW$Ano, dfTotalGW$AnoparaRev, col='blue', pch=19)



# Calculando as médias mensais

df$Mes<- as.integer(df$Mes)

count1<-1
dfMediaMensal<-data.frame()

for (i in unique(df$Ano)){
  aaa<-as.data.frame(df[df$Ano==i,])
  for(j in unique(aaa$Mes)){
    bbb<-mean(aaa[aaa$Mes==j,5])
    bbb<-round(bbb,2)
    dfMediaMensal[count1,1]<-i
    dfMediaMensal[count1,2]<-j
    dfMediaMensal[count1,3]<-bbb
    count1<-count1+1
  }
  
}
  
names(dfMediaMensal)<-c("Ano","Mes","Consumo_MW")  
dfMediaMensal
dim(dfMediaMensal)
dfMediaMensal$Contador<-1:277

plot(dfMediaMensal$Contador,dfMediaMensal$Consumo_MW, col='blue', pch=19)



# Médias por cada mês
dfMediaMensal$Contador<-NULL
dfMediaJAN<-dfMediaMensal[dfMediaMensal$Mes==1,]
dfMediaFEV<-dfMediaMensal[dfMediaMensal$Mes==2,]
dfMediaMAR<-dfMediaMensal[dfMediaMensal$Mes==3,]
dfMediaABR<-dfMediaMensal[dfMediaMensal$Mes==4,]
dfMediaMAI<-dfMediaMensal[dfMediaMensal$Mes==5,]
dfMediaJUN<-dfMediaMensal[dfMediaMensal$Mes==6,]
dfMediaJUL<-dfMediaMensal[dfMediaMensal$Mes==7,]
dfMediaAGO<-dfMediaMensal[dfMediaMensal$Mes==8,]
dfMediaSET<-dfMediaMensal[dfMediaMensal$Mes==9,]
dfMediaOUT<-dfMediaMensal[dfMediaMensal$Mes==10,]
dfMediaNOV<-dfMediaMensal[dfMediaMensal$Mes==11,]
dfMediaDEZ<-dfMediaMensal[dfMediaMensal$Mes==12,]


plot(dfTotal$Ano, dfTotal$Media_Anual, col='red', pch=19)
points(dfMediaFEV$Ano,dfMediaFEV$Consumo_MW, col='blue', pch=19)

