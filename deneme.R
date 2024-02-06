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

#### googlesheet'ten veri çekme ####

toto<-read_sheet("https://docs.google.com/spreadsheets/d/1kPf7F2MQknLMBM_q2-AHxK1aclzVpwZuus4dOkkKM4s/edit?usp=sharing")
toto<-toto%>%
  select(c(1,2,5,6,9,10,11))
toto<-toto[6:2035,]
colnames(toto)<-c("Sira", "Mac", "Sonuc", "Mac_gunu", "Osman", "Okan", "Rasim")
toto2<-na.omit(toto)
toto2<-filter(toto2, toto2$Sira!="Sıra")
toto2<-filter(toto2, toto2$Sira!="NULL")
toto2$hafta<-c(sort(rep(1:(nrow(toto2)/15),15)))
toto2<-as.data.frame(toto2)

totoguncel<-write.xlsx2(toto2,"totoguncel.xlsx")

#### googlesheet'ten hesaplama yapma #####

tahmin2<-filter(toto2, max(toto2$hafta)==toto2$hafta) 
tahmin2turk<-tahmin2[1:10,]
tahmin2yab<-tahmin2[11:15,]

oranlar<-data.frame(Osman=c(0.45, 0.68, 0.57, 0.75), Okan=c(0.5, 0.6, 0.58, 0.73), Rasim=c(0.4, 0.66, 0.46, 0.7))
row.names(oranlar)<-c("turk_tek","turk_cift","yab_tek", "yab_cift")

dogrusaytek<-data.frame(NA)
dogrusay2<-data.frame(NA)

for(i in 1:10){
for (j in 1:3){  
  if (tahmin2turk[i,(j+4)]<10){
    dogrusaytek[i,j]<-1
}
    else{
      dogrusaytek[i,j]<-2
  }
  }
}

tektopla<-data.frame(NA)
for(i in 1:10){
  for (j in 1:3){  
    if (dogrusaytek[i,j]==1){
     tektopla[i,j] <- dogrusaytek[i,j]*oranlar[1,j]
    }
    else{
      tektopla[i,j]<-(dogrusaytek[i,j]*oranlar[2,j])/2
    }
  }
}
dogrusay2<-ceiling(colSums(tektopla))

kontroltablo2<-matrix(NA,nrow(tahmin2turk),choose(nrow(tahmin2turk),dogrusay[1]))
ltek2 <- rep(list(olastek), nrow(tahmin2turk))
atrtek2<-expand.grid(ltek2)
tatrtek2<-as.data.frame(t(atrtek2))

as2<-as.data.frame(combn(tahmintek$kod, dogrusay))

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



##### 06022024 #####

#### tekler için hesaplama ####
oranlar<-data.frame(Osman=c(0.45, 0.68, 0.57, 0.75), Okan=c(0.5, 0.6, 0.58, 0.73), Rasim=c(0.4, 0.66, 0.46, 0.7))
row.names(oranlar)<-c("turk_tek","turk_cift","yab_tek", "yab_cift")

dogrusaytek<-data.frame(NA)
for(i in 1:10){
for (j in 1:3){  
  if (tahmin2turk[i,(j+4)]<10){
    dogrusaytek[i,j]<-1
}
    else{
      dogrusaytek[i,j]<-2
  }
  }
}

tektopla<-data.frame(NA)
for(i in 1:10){
  for (j in 1:3){  
    if (dogrusaytek[i,j]==1){
     tektopla[i,j] <- dogrusaytek[i,j]*oranlar[1,j]
    }
    else{
      tektopla[i,j]<-(dogrusaytek[i,j]*oranlar[2,j])/2
    }
  }
}
dogrusay2<-data.frame(NA)
dogrusay2<-ceiling(colSums(tektopla))
dogrusay2tek<-ceiling(colSums(filter(tektopla,tektopla$NA.==0.45)))

tahmintekosman<-filter(tahmin2turk, tahmin2turk$Osman<10)
kontroltablo2<-matrix(NA,nrow(tahmintekosman),choose(nrow(tahmintekosman),dogrusay2tek[1]))
ltek2 <- rep(list(olastek), nrow(tahmintekosman))
atrtek2<-expand.grid(ltek2)
tatrtek2<-as.data.frame(t(atrtek2))



as2<-as.data.frame(combn(tahmintekosman$Sira, dogrusay2tek[1]))

for (i in 1:nrow(as2)){ 
  for(j in 1:ncol(as2)){
    if (as2[i,j]==1) {
      kontroltablo2[1,j]=1}
    else if (as2[i,j]==2) {
      kontroltablo2[2,j]=2}
    else if(as2[i,j]==3) {
      kontroltablo2[3,j]=1}
    else if (as2[i,j]==5) {
      kontroltablo2[4,j]=2}
    else if (as2[i,j]==8) {
      kontroltablo2[5,j]=1}
    else  {kontroltablo2[6,j]=2}
  }
}


olasılıklar<-data.frame(NA)
for (i in 1:ncol(kontroltablo2)){ 
  for(j in 1:ncol(tatrtek2)){
    if (sum(kontroltablo2[,i]==tatrtek2[,j], na.rm=TRUE)==3 & sum(tatrtek2[,j]==tahmintekosman$Osman)!=6){
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
  unique<-as.vector(sort(unique(as.vector(as.matrix(olasılıklar)))))
}

secilenlertek<-tatrtek2%>%
  select(all_of(unique))%>%
  cbind(sira=tahmintekosman$Sira)


### çiftler için hesaplama
olascift<-c(10,12,20)
tahmincift<-data.frame(sira=c("1.mac","2.mac","3.mac","4.mac"),tahmin=c(10,12,20,10), kod=c(4,6,7,9))
dogrusay<-3
n<- 4
kontroltablo<-matrix(NA,n,choose(n,dogrusay))
l <- rep(list(olascift), n)
atr<-expand.grid(l)
tatr<-as.data.frame(t(atr))

as<-as.data.frame(combn(tahmincift$kod, dogrusay))
for (i in 1:nrow(as)){ 
  for(j in 1:ncol(as)){
    if (as[i,j]==4) {
      kontroltablo[1,j]=10}
    else if (as[i,j]==6) {
      kontroltablo[2,j]=12}
    else if(as[i,j]==7) {
      kontroltablo[3,j]=20}
    else  {
      kontroltablo[4,j]=10}
    
  }
}

olasılıklar<-data.frame(NA)
for (i in 1:ncol(kontroltablo)){ 
  for(j in 1:ncol(tatr)){
    if (sum(kontroltablo[,i]==tatr[,j], na.rm=TRUE)==3 & sum(tatr[,j]==tahmincift$tahmin)!=4){
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
  unique<-as.vector(sort(unique(as.vector(as.matrix(olasılıklar)))))
}

secilenlercift<-tatr%>%
  select(all_of(unique))%>%
  cbind(sira=tahmincift$kod)



