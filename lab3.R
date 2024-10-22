rm(list=ls())
cat("\014")
dev.off()

dane <- read.table("http://www.kozlowski.pollub.pl/kukurydza.txt",sep="\t",header=T)

head(dane)
tail(dane)
cena <- dane$Zamkniecie
plot(cena,main="ceny",lwd=2,type="l",xlab="czas",ylab="PLN")

cena <- cena[seq(from=length(cena),to=1,by=-1)]

czas <- 1:length(cena)
plot(cena,main="ceny",lwd=2,type="l",xlab="czas",ylab="PLN")

ruchoma <- function(x,m,kolor){
  t <- length(x)
  f <- NULL
  for(i in (m+1):t){
    f[i] <- mean(x[(i-m):i])
  }
  lines((m+1):t,f[(m+1):t],col=kolor,lwd=2)}

plot(cena,main="ceny",lwd=2,type="l",xlab="czas",ylab="PLN")
ruchoma(cena,2,"red")

plot(cena,main="ceny",lwd=2,type="l",xlab="czas",ylab="PLN")
ruchoma(cena,5,"blue")

plot(cena,main="ceny",lwd=2,type="l",xlab="czas",ylab="PLN")
ruchoma(cena,11,"green")

plot(cena,main="ceny",lwd=2,type="l",xlab="czas",ylab="PLN")
ruchoma(cena,23,"pink")

plot(cena,main="ceny",lwd=2,type="l",xlab="czas",ylab="PLN")
ruchoma(cena,9,"skyblue")

