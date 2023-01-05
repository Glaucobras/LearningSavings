# Script to save some functions created by Glauco G. Silveira




# Function to multiply all elements of 2 vectors element by element.
# It will create a new vector with lenght iqual to lenght(vec1)*lenght(vec2)
vecmultiply <- function (vector1,vector2){
  a<-c()
  count<-1
  count3<-1
  for (i in vector1) {
    for (b in vector2){
      a[count3]<-(b*i)
      count3<-count3+1
    }
    
  }
  
  print(a)
  
}

#Example :
vec1<-c(1,2,3,4)
vec2<-c(10,20,30,40)

vecmultiply(vec1,vec2)
