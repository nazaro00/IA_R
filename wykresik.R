#Określamy folder roboczy
setwd("C:/Users/User/Desktop/IA_R")
#Wczytujemy dane z pliku stzwr.csv
stzwr <- read.csv(file = "stzwr.csv", sep=";", dec=",") #domyślnie header = TRUE
#Wybieramy do analizy dwa aktywa i zapisujemy pod nazwą nowej zmiennej
portfel2 <- stzwr[c(3,5)]
#Definujemy zmienne do obliczeń
srebro <- portfel2$Srebro
pallad <- portfel2$Pallad
#Ustalamy wagi portfela
w1 <- seq(from = 0.00, to = 1, by = 0.01)
w2 <- 1-w1


#Obliczamy odchylenia standardowe
s1 <- sd(srebro)
s2 <- sd(pallad)
#Obliczanie korelacji
korelacja <- cor(srebro,pallad)
#Stopa zwrotu portfela
iportfela <- mean(srebro)*w1+mean(pallad)*w2
#ryzyko portfela
ryzykoportfela <- (w1^2*s1^2+w2^2*s2^2+2*w1*w2*s1*s2*korelacja)^0.5
#Rysujemy wykres
plot(ryzykoportfela,iportfela, type= "p", col = "green")
title(main="Zbiór możliwości inwestycyjnych 2 aktywa")
