
crossing(var1=0:2, var2=0:2, var3=0:2, var4=0:2, var5=0:2)

dbinom(x=5, size= 5, prob = 1/3)
combn(10,5, min)

combn(c(1,2,3,4,5), 3)


(m <- combn(10, 5, min))   # minimum value in each combination

mm <- as.data.frame(combn(5, 3, function(x) matrix(x, 2, 3)))


## Different way of encoding points:
combn(c(1,1,1,1,2,2,2,3,3,4), 3, tabulate, nbins = 4, simplify = FALSE)

## Compute support points and (scaled) probabilities for a
## Multivariate-Hypergeometric(n = 3, N = c(4,3,2,1)) p.f.:
table.mat(t(combn(c(1,1,1,1,2,2,2,3,3,4), 3, tabulate, nbins = 4)))

## Assuring the identity
for(n in 1:7)
  for(m in 0:n) stopifnot(is.array(cc <- combn(n, m)),
                          dim(cc) == c(m, choose(n, m)),
                          identical(cc, combn(n, m, identity)) || m == 1)



as<-crossing(var1 = c(0,1,2), var2 = c(0,1,2), var3 = c(0,1,2))

expand(mtcars, nesting(vs, cyl, gear))

factor(mtcars$cyl)

##########
fruits <- tibble(
  type = c("apple", "orange", "apple", "orange", "orange", "orange"),
  year = c(2010, 2010, 2012, 2010, 2011, 2012),
  size = factor(
    c("XS", "S", "M", "S", "S", "M"),
    levels = c("XS", "S", "M", "L")
  ),
  weights = rnorm(6, as.numeric(size) + 2)
)

fruits%>%expand(type)
fruits%>%expand(type, size, year)
fruits %>% expand(type, size, full_seq(year, 1))
all <- fruits %>% expand(type, size, year)
fruits %>% dplyr::right_join(all)



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

olasılıklar3<-na.omit(olasılıklar)







require(gtools)
df1 <- data.frame(permutations(n = 4, r = 4, v = 1:4)) %>% cbind(sample(1:24))
colnames(df1) <- c("Apples", "Pears", "Lemons", "Oranges", "Happiness")

df2 = data.frame(Apples = c(1, 3, 2, 4), Pears = c(4, 1, 1, 3), Lemons = c(2, 2, 3, 1), Oranges = c(3, 4, 4, 2))



