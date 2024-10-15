rm(list=ls())
cat("\014")
graphics.off()

#pierwsze zadanie 
x <- c(2.3, 7, 9, 12 , 1 , 6, 20, 0.3, 5)
plot(x, type = 'b', lwd = 2, pch =20, cex = 1.5, col= 3, 
     lty =2, xlim =c(0,10), ylim = c(-2,30), main = "Wykres 1", 
     col.main = "blue", col.axis = "red")
x
length(x)
macierz <- matrix(x, ncol = 3, byrow = TRUE)
macierz 
macierz1 <- matrix(x, ncol = 3, byrow = FALSE)
macierz1
t(macierz)

t(macierz) == macierz1
all.equal(t(macierz), macierz1)


x
length(x)
dim(macierz)
graphics.off()

#zadanie 2
dane <- read.table("http://www.kozlowski.pollub.pl/kolokwium.txt", sep = "\t", header = TRUE)
head(dane, n = 8 )
str(dane)
dane$BRENT
dane[,2]
all.equal(dane[,2], dane$BRENT)
tail(dane, n =5)

ropa <- dane$BRENT
hist(ropa, breaks = 15, col =4, main = "Histogram cen ropy BRENT", xlab = "Cena USD", ylab ="Liczebność", prob=FALSE)

hist(ropa, breaks = 15, col =4, main = "Histogram cen ropy BRENT", xlab = "Cena USD", ylab ="Liczebność", prob=TRUE)

gestosc <-density(ropa)
lines(x = gestosc$x, y = gestosc$y, col = "green", lwd = 2)
arrows(x0 = 30, y0 = 0.06, x1 = 43, y1 = 0.04, col = "red", lwd = 1.5)
text(x=30, y = 0.063, pos = 3, labels = "gęstość")


z <-  locator(2)
z
arrows(x0=z$x[1], y0 = z$y[1], x1=z$x[2], y1 = z$y[1], col = 3)

##################################
boxplot(ropa, col ='blue', main ='Wykres ramka-wąsy', horizontal = TRUE)

summaru(ropa)
mean(ropa)
var(ropa)
sd(ropa)
quantile(ropa, c(0.05, 0.25, 0.5, 0.75, 0.95))
seq(from =0, to =1, by = 0.05)
quantile(ropa, seq(from =0, to =1, by = 0.05))
boxplot(ropa, col = 6, main ="Ramka - wąsy dla cen ropy")

qqnorm(ropa, col = 'blue')
qqline(ropa, col = 2, lwd = 2)
shapiro.test(ropa)
e1071::skewness(ropa, type=1)
e1071::kurtosis(ropa, type=1)

##################################

old_par <- par(no.readonly = T)
par(mfrow = c(1,2))
hist(ropa, breaks = 15, col = 4, main = "Histogram cen ropy Brent",
     xlab = "Cena, USD", ylab = 'Liczebność')
box()
boxplot(ropa, col = 6, main = "Ramka - wąsy dla cen ropy")
box()
par(old_par)


kawa <- dane$COFFEE
old_par <-par(no.readonly = T)
par(mfrow = c(2,2))
plot(kawa, type = "l", main = "Cechy kawy", 
     xlab = "notowania", ylab= "USD")
boxplot(kawa, col = 6, main = "Ramka - wąsy dla cen kawy")
hist(kawa, breaks = 15, col =4,  main = "Histogram cen kawy", 
     xlab = "Cena, USD", ylab = 'Liczebność')
plot(density(kawa), main = "Funkcja gęstości", type = "h", col = 2)
par(old_par)


