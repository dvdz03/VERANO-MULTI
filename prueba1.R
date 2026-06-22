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
tierra<-c(114,111,103,111,116,116,111,106,113,118,112,117,114,116,118,107,119,120,122,117,118,123,117,112,110,122,121,121,116,107,107,112,115,120,117,121)
length(tierra)
mean(tierra)
115*0.30
