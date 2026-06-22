#Proyecto final
sample(prub,18)
sample(prub2,18)
prub2<-c(19:36)
prub<-c(1:18)
matriz1<-sample(length(matrix(prub,nrow=2,ncol=9)),18)
as.matrix(matriz1,nrow=2,ncol=9)

#AHORA SI
luzdifusa<-c(1,2,3,4,5,6,7,8,9,28,31,35,24,27,36,20,23,29)
length(luzdifusa)
luzdirecta<-c(10,11,12,13,14,15,16,17,18,19,21,22,25,26,30,32,33,34)
length(luzdirecta)
anyDuplicated(luzdifusa&luzdirecta)
intersect(luzdifusa,luzdirecta)
sample(luzdifusa,18)
sample(luzdirecta,18)
