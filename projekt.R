install.packages("dplyr")
install.packages("tidyverse")
install.packages("quadprog")
install.packages("psych")

library(dplyr)
library(tidyverse)
library(quadprog)




# Import data set
data <- read.csv("stopa.csv", header = TRUE, sep = ";")
data$Cocoa <- as.numeric(data$Cocoa)
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

data_log <- data.frame(Date = data$Date[-1],
                      Cocoa = diff(log(data$Cocoa)),
                      Allegro = diff(log(data$Allegro)),
                      NERD = diff(log(data$NERD)),
                      XRP = diff(log(data$XRP)))

# Descriptive statistics – need to transpond and create a table
library(psych)
describe(data_log[,-1])


#Variables for all calculations
Cocoa <- data_log$Cocoa
Allegro <- data_log$Allegro
NERD <- data_log$NERD
XRP <- data_log$XRP

# Plot of Cocoa log return rates
ggplot(data_log, aes(Date, Cocoa)) + 
  geom_line(color = "chocolate4", linewidth = 1.2) + 
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  ggtitle("Wykres logarytmicznych stóp zwrotu - kakao")

# Plot of Allegro log return rates
ggplot(data_log, aes(Date, Allegro)) + 
  geom_line(color = "darkorange", linewidth = 1.2) + 
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  ggtitle("Wykres logarytmicznych stóp zwrotu - Allegro")

# Plot of NERD log return rates
ggplot(data_log, aes(Date, NERD)) + 
  geom_line(color = "darkorchid", linewidth = 1.2) + 
  scale_y_continuous(labels = scales::percent) + 
  theme_light() +
  ggtitle("Wykres logarytmicznych stóp zwrotu - Roundhill Video Games")

# Plot of XRP log return rates
ggplot(data_log, aes(Date, XRP)) + 
  geom_line(color = "red3", linewidth = 1.2) + 
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  ggtitle("Wykres logarytmicznych stóp zwrotu - XRP")

weights4inv <- read.table("weights4inv.txt",dec=",", header=TRUE, quote="\"",stringsAsFactors=FALSE)
w1 <- weights4inv$W1
w1 <- as.numeric(w1)
w2 <- weights4inv$W2
w2 <- as.numeric(w2)
w3 <- weights4inv$W3
w3 <- as.numeric(w3)
w4 <- weights4inv$W4
w4 <- as.numeric(w4)

#calculating SD
s1 <- sd(Cocoa)
s2 <- sd(Allegro)
s3 <- sd(NERD)
s4 <- sd(XRP)

#Calculating correlation
corr12 <- cor(Cocoa, Allegro)
corr13 <- cor(Cocoa, NERD)
corr14 <- cor(Cocoa, XRP)
corr23 <- cor(Allegro, NERD)
corr24 <- cor(Allegro, XRP)
corr34 <- cor(NERD, XRP)


# Calculate covariation matrix
cov_matrix <- cov(data_log[,-1]) # weak covariation indicates little to no correlation between the assets, great diversification

# Function for minimum risk portfolio based on expected returns and covariance matrix
optimize_portfolio <- function(expected_returns, cov_matrix) {
  n <- length(expected_returns) # number of assets in portfolio
  Dmat <- 2 * cov_matrix # multiply the covariance matrix by 2 for the quadratic programming formulation
  dvec <- rep(0, n) # create a vector of n zeros for the linear term in the quadratic programming formulation
  Amat <- cbind(1, diag(n)) # create constaint matrix 
  bvec <- c(1, rep(0, n)) # create constraint vector, 1 ensures the sum of weights = 1, followed by n zeros for the inequality constraints
  solve.QP(Dmat, dvec, Amat, bvec, meq = 1) # solve the quadratic programming problem
}

# Wyznaczanie minimalnego ryzyka
min_risk <- optimize_portfolio(rep(0, ncol(data_log)-1), cov_matrix)
min_risk_weights <- min_risk$solution

# Function for maximum efficacy portfolio (Sharpe ratio)
maximize_sharpe <- function(expected_returns, cov_matrix, risk_free_rate = 0) {
  n <- length(expected_returns)
  Dmat <- 2 * cov_matrix
  dvec <- expected_returns - risk_free_rate
  Amat <- cbind(1, diag(n))
  bvec <- c(1, rep(0, n))
  solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
}

# Wyznaczanie maksymalnej efektywności
expected_returns <- colMeans(data_log[,-1]) * 252
max_efficiency <- maximize_sharpe(expected_returns, cov_matrix)
max_efficiency_weights <- max_efficiency$solution

weights <- data.frame(min_risk_weights, max_efficiency_weights)

#calculating ip
iportfolio <- mean(Cocoa)*w1+mean(Allegro)*w2+mean(NERD)*w3+mean(XRP)*w4

#portfolio risk
sdp <- (w1^2*s1^2 + w2^2*s2^2 + w3^2*s3^2 + w4^2*s4^2 + 2*w1*w2*s1*s2*corr12 + 2*w1*w3*s1*s3*corr13 + 2*w1*w4*s1*s4*corr14 + 
          2*w2*w3*s2*s3*corr23 + 2*w2*w4*s2*s4*corr24 + 2*w3*w4*s3*s4*corr34)^0.5

#calculating effectivness
rf <- 0.0
sharp <- (iportfolio-rf)/sdp

#preparing df with results
results <- cbind(w1, w2, w3, w4, iportfolio, sdp, sharp)
results <- as.data.frame(results)

#finding interesting portfolios
min.risk <- subset(results, results$sdp==min(results$sdp))
max.effectivness <- subset(results, results$sharp==max(results$sharp))
max.ip <- subset(results, results$iportfolio==max(results$iportfolio))
max.w1 <- subset(results, results$w1==1)
max.w2 <- subset(results, results$w2==1)
max.w3 <- subset(results, results$w3==1)
max.w4 <- subset(results, results$w4==1)
des <- c("Minimal risk portfolio", "Maximum efficiency portfolio", "Maximum rate of return portfolio", "Max weight one portfolio", "Max weight two portfolio", "Max weight three portfolio", "Max weight four portfolio")

#Creating table with results 3 portfolios and showing results in console
df <- cbind(rbind(min.risk, max.effectivness, max.ip, max.w1, max.w2, max.w3, max.w4), des)
df

write.csv(x=results, file = "portfolio.csv", row.names=FALSE)
#creating and saving OS
plot(sdp, iportfolio, type= "p", col = "red")

title(main="Opportunity set for four risky assets without SS")
points(min.risk$sdp, min.risk$iportfolio, pch=19, col="green")
points(max.effectivness$sdp, max.effectivness$iportfolio, pch=19, col="blue")
points(max.ip$sdp, max.ip$iportfolio, pch=19, col="yellow")
points(max.w1$sdp, max.w1$iportfolio, pch=19, col="black")
points(max.w2$sdp, max.w2$iportfolio, pch=19, col="black")
points(max.w3$sdp, max.w3$iportfolio, pch=19, col="black")
points(max.w4$sdp, max.w4$iportfolio, pch=19, col="black")
legend(legend = c("Opportunity set without SS", "Minimum risk portfolio", "Maximum efficiency portfolio", "Maximum RoR portfolio", 
                  "One-element portfolio"), 
       pch = c(19, 19, 19, 19, 19), 
       col = c("red", "green", "blue", "yellow", "black"), 
       "right")
dev.copy(png, filename="plot.png")
dev.off ()
