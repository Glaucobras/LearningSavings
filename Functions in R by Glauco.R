# Script to save some functions created by Glauco G. Silveira


# 1

# Function to multiply all elements of 2 vectors element by element.
# It will create a new vector with lenght iqual to lenght(vec1)*lenght(vec2)

# Função para multiplicar dois vetores elemento a elemento criando um vetor
# com comprimento maior
# Palavras chave: multiplicação multiplicar vetores elementos


vecmultiply <- function (vector1,vector2){
  a<-c()
  count<-1
  count2<-1
  for (i in vector1) {
    for (b in vector2){
      a[count2]<-(b*i)
      count2<-count2+1
    }
    
  }
  
  return(a)
  
}

#Example :
vec1<-c(1,2,3,4,5,6,7,8,9)
vec2<-c(10,20,30,40)


vecmultiply(vec1,vec2)


# 2 

# Function that calculates the mode in a vector. Mode is the value that is most observable in a vector.
# Função que calcula a moda em um vetor.

# Palavras chave : moda

moda = function(dados) {
  vetor = table(as.vector(dados))
  names(vetor)[vetor == max(vetor)]
}

