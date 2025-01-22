#importing data
ror <- read.csv("ror.csv", sep=";", dec=",")
#We choose three assets for further analysis 
portfolio3 <- ror[c(1, 2, 3)] 

#na kole może dać inny plik z danymi, mogą tam być polskie znaki czy złe formatowanie w jakiś inny sposób
#trzeba będzie zmienić numery kolumn, ich nazwy
#wskaż zbiór możliwości inwestycyjnych dla tego portfela
#

#Variables for all calculations
wig <- portfolio3$WIG
gold <- portfolio3$Gold
silver <- portfolio3$Silver
#importing weights from file
weights3inv <- read.table("weights3invSS.txt",dec=",", header=TRUE, quote="\"",stringsAsFactors=FALSE)
w1 <- weights3inv$W1
w1 <- as.numeric(w1)
w2 <- weights3inv$W2
w2 <- as.numeric(w2)
w3 <- weights3inv$W3
w3 <- as.numeric(w3)
#calculating SD
s1 <- sd(wig)
s2 <- sd(gold)
s3 <- sd(silver)
#Calculating corellation
corr12 <- cor(wig,gold)
corr13 <- cor(wig,silver)
corr23 <- cor(gold, silver)
#calculating ip
iportfolio <- mean(wig)*w1+mean(gold)*w2+mean(silver)*w3
#portfolio risk
sdp <- (w1^2*s1^2 + w2^2*s2^2 + w3^2*s3^2 + 2*w1*w2*s1*s2*corr12 + 2*w1*w3*s1*s3*corr13 + 2*w2*w3*s2*s3*corr23)^0.5
#calculating effectivness
rf <- 0.0
sharp <- (iportfolio-rf)/sdp
#preparing df with results
data <- cbind(w1, w2, w3, iportfolio, sdp, sharp)
data <- as.data.frame(data)
#finding interesting portfolios
min.risk <- subset(data, data$sdp==min(data$sdp))
max.effectivness <- subset(data, data$sharp==max(data$sharp))
max.ip <- subset(data, data$iportfolio==max(data$iportfolio))
max.w1 <- subset(data, data$w1==1 & data$w2==0 & data$w3==0)
max.w2 <- subset(data, data$w1==0 & data$w2==1 & data$w3==0)
max.w3 <- subset(data, data$w1==0 & data$w2==0 & data$w3==1)
des <- c("Minimal risk portfolio", "Maximum efficiency portfolio", "Maximum rate of return portfolio", "Max weight one portfolio", "Max weight two portfolio", "Max weight three portfolio")
#Creating table with results 3 portfolios and showing results in console
results <- cbind(rbind(min.risk, max.effectivness, max.ip, max.w1, max.w2, max.w3), des)
results
write.csv(x=results, file = "results.csv", row.names=FALSE)
#creating and saving OS
plot(sdp, iportfolio, type= "p", col = "red")

datawithoutSS <-subset(data, data$w1>=0)
datawithoutSS <-subset(datawithoutSS, datawithoutSS$w2>=0)
datawithoutSS <-subset(datawithoutSS, datawithoutSS$w3>=0)

points(datawithoutSS$sdp,datawithoutSS$iportfolio, col = "purple")

#ten fragment pokazuje charakterystyki portfeli optymalnych i tworzy dla nich plik results, z tego pliku można odczytać różny portfele i ich
#optymalne wagi
title(main="Opportunity set for three risky assets with SS")
points(min.risk$sdp, min.risk$iportfolio, pch=19, col="green")
points(max.effectivness$sdp, max.effectivness$iportfolio, pch=19, col="blue")
points(max.ip$sdp, max.ip$iportfolio, pch=19, col="yellow")
points(max.w1$sdp, max.w1$iportfolio, pch=19, col="black")
points(max.w2$sdp, max.w2$iportfolio, pch=19, col="black")
points(max.w3$sdp, max.w3$iportfolio, pch=19, col="black")
legend(legend = c("Opportunity set with SS", "Opportunity set without SS", "Minimum risk portfolio", "Maximum efficiency portfolio", "Maximum RoR portfolio", 
                  "One-element portfolio"), 
       pch = c(19, 19, 19, 19, 19, 19), 
       col = c("red", "purple", "green", "blue", "yellow", "black"), 
       "right")
dev.copy(png, filename="plot.png")
dev.off ()
