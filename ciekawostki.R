#importowanie danych z pliku CSV

tabela <- read.csv("stzwr.csv", header= TRUE, sep=";", dec=",")

#uważać czy kolumny mają wartości liczbowe, bo procenty się nie wczytają tak jak powinny

tabela[c(1:2, 4), ]
