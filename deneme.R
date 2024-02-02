###################
#### UYGULAMA #####
###################
rm(list = ls())
library(tidyr)
library(dplyr)
library(gtools)
library(readxl)

olastek<-c(0,1,2)
olascift<-c(10,12,20)
tahmintek<-data.frame(sira=c("1.mac","2.mac","3.mac","4.mac","5.mac"),tahmin=c(1,0,2,0,1), kod=c("a","b","c","d","e"))
dogrusay<-3
n<- 5
kontroltablo<-matrix(NA,n,choose(n,dogrusay))
l <- rep(list(olastek), n)
atr<-expand.grid(l)
tatr<-as.data.frame(t(atr))
write.csv2(atr, "atr.csv")

as<-as.data.frame(combn(tahmintek$kod, dogrusay))

for (i in 1:nrow(as)){ 
  for(j in 1:ncol(as)){
  if (as[i,j]=="a") {
    kontroltablo[1,j]=1}
    else if (as[i,j]=="b") {
      kontroltablo[2,j]=0}
      else if(as[i,j]=="c") {
        kontroltablo[3,j]=2}
        else if (as[i,j]=="d") {
          kontroltablo[4,j]=0}
          else  {kontroltablo[5,j]=1}
  }
}
      
write.csv(kontroltablo,"kontroltablo.csv")
  
olasılıklar<-data.frame(NA)


for (i in 1:ncol(kontroltablo)){ 
  for(j in 1:ncol(tatr)){
    if (sum(kontroltablo[,i]==tatr[,j], na.rm=TRUE)==3){
      olasılıklar[i,j]<-j
    } else{
      olasılıklar[i,j]<-NA
    }
  }
}  

filtre_olas<-data.frame(NA)

for (i in 1:ncol(olasılıklar)){ 
  for(j in 1:ncol(olasılıklar)){
    if (is.na(olasılıklar[i,j])==FALSE){
      filtre_olas[,]<-olasılıklar[i,j]}  
      
  }
  unique<-as.data.frame(sort(unique(as.vector(as.matrix(olasılıklar)))))
}











