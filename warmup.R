#tworzenie wektora z danymi
dane <- c(10, -0.5, 12, 3.000)
dane
dane <- dane/2

#też działa, ale nie używamy tego
dane = dane/2 

#= jest zarezerwowane dla wprowadzania argumentów funkcji 

mean <- mean(x = dane)

#funkcja ()

beznadziejna_stopa <- function(FV, PV) {(FV-PV)/PV}
beznadziejna_stopa(FV = 110, PV = 100)
beznadziejna_stopa(FV <- 110, PV <- 100)
