rm(list=ls())
library(stringr)
library(ggplot2)
library(DataExplorer)
library(tseries)
library("TTR")


df <- read.csv("all_merged_done.csv") # Reading the number of cases
df <- subset(df,select = -c(1))


View(df)

ggplot() + 
  geom_line(aes(x=df$Date,y=df$total_cases,group = 1),color='blue') + 
  geom_line(aes(x=df$Date,y=df$total_deaths,group = 1),color='red') +
  geom_line(aes(x=df$Date,y=df$total_strains,group = 1),color='green') +
  geom_line(aes(x=df$Date,y=df$Total_severity,group = 1),color='purple') +
  ylab('Values')+xlab('date')


df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}



#function to calculate Gaussian kernel
gausinKernel <- function(x,b){
  K <- (1/((sqrt(2*pi))))*exp(-0.5 *(x/b)^2)
  return(K)
}

# ------------------------------------------------------------------------------
# -------------------KSmoothening for Total number of cases---------------------
# ------------------------------------------------------------------------------
x <- df$x
y <- df$total_cases




b <- 10 #bandwidth
kdeEstimateyX <- seq(0,510,1)
ykernel <- NULL
for(xesti in kdeEstimateyX){
  xx <-  xesti - x
  K <-gausinKernel(xx,b)
  Ksum <- sum(K)
  weight <- K/Ksum
  yk <- sum(weight*y)
  xkyk <- c(xesti,yk)
  ykernel <- rbind(ykernel,xkyk)
}
plot(x,y,xlab = "Time", ylab = "Number of Total Cases", col = 'blue', cex = 2)
lines(ykernel[,1],ykernel[,2], col = 'red', lwd = 2)


b <- 5 #bandwidth
kdeEstimateyX <- seq(0,510,1)
ykernel <- NULL
for(xesti in kdeEstimateyX){
  xx <-  xesti - x
  K <-gausinKernel(xx,b)
  Ksum <- sum(K)
  weight <- K/Ksum
  yk <- sum(weight*y)
  xkyk <- c(xesti,yk)
  ykernel <- rbind(ykernel,xkyk)
}
plot(x,y,xlab = "Time", ylab = "Number of Total Cases", col = 'blue', cex = 2)
lines(ykernel[,1],ykernel[,2], col = 'red', lwd = 2)


b <- 15 #bandwidth
kdeEstimateyX <- seq(0,510,1)
ykernel <- NULL
for(xesti in kdeEstimateyX){
  xx <-  xesti - x
  K <-gausinKernel(xx,b)
  Ksum <- sum(K)
  weight <- K/Ksum
  yk <- sum(weight*y)
  xkyk <- c(xesti,yk)
  ykernel <- rbind(ykernel,xkyk)
}
plot(x,y,xlab = "Time", ylab = "Number of Total Cases", col = 'blue', cex = 2)
lines(ykernel[,1],ykernel[,2], col = 'red', lwd = 2)



lm1 <- lm(ykernel[,2] ~ ykernel[,1])


coef(lm1)


# check R-squared values
summary(lm1)$r.squared


# ------------------------------------------------------------------------------
# -------------------KSmoothening for Total number of deaths--------------------
# ------------------------------------------------------------------------------

x <- df$x
y <- df$total_deaths




b <- 10 #bandwidth
kdeEstimateyX <- seq(0,510,1)
ykernel <- NULL
for(xesti in kdeEstimateyX){
  xx <-  xesti - x
  K <-gausinKernel(xx,b)
  Ksum <- sum(K)
  weight <- K/Ksum
  yk <- sum(weight*y)
  xkyk <- c(xesti,yk)
  ykernel <- rbind(ykernel,xkyk)
}
plot(x,y,xlab = "Time", ylab = "Number of Total Deaths", col = 'blue', cex = 2)
lines(ykernel[,1],ykernel[,2], col = 'red', lwd = 2)


b <- 5 #bandwidth
kdeEstimateyX <- seq(0,510,1)
ykernel <- NULL
for(xesti in kdeEstimateyX){
  xx <-  xesti - x
  K <-gausinKernel(xx,b)
  Ksum <- sum(K)
  weight <- K/Ksum
  yk <- sum(weight*y)
  xkyk <- c(xesti,yk)
  ykernel <- rbind(ykernel,xkyk)
}
plot(x,y,xlab = "Time", ylab = "Number of Total Deaths", col = 'blue', cex = 2)
lines(ykernel[,1],ykernel[,2], col = 'red', lwd = 2)


b <- 15 #bandwidth
kdeEstimateyX <- seq(0,510,1)
ykernel <- NULL
for(xesti in kdeEstimateyX){
  xx <-  xesti - x
  K <-gausinKernel(xx,b)
  Ksum <- sum(K)
  weight <- K/Ksum
  yk <- sum(weight*y)
  xkyk <- c(xesti,yk)
  ykernel <- rbind(ykernel,xkyk)
}
plot(x,y,xlab = "Time", ylab = "Number of Total Deaths", col = 'blue', cex = 2)
lines(ykernel[,1],ykernel[,2], col = 'red', lwd = 2)



lm1 <- lm(ykernel[,2] ~ ykernel[,1])


coef(lm1)


# check R-squared values
summary(lm1)$r.squared

# ------------------------------------------------------------------------------
# -------------------KSmoothening for Total number of strains--------------------
# ------------------------------------------------------------------------------
x <- df$x
y <- df$total_strains




b <- 10 #bandwidth
kdeEstimateyX <- seq(0,510,1)
ykernel <- NULL
for(xesti in kdeEstimateyX){
  xx <-  xesti - x
  K <-gausinKernel(xx,b)
  Ksum <- sum(K)
  weight <- K/Ksum
  yk <- sum(weight*y)
  xkyk <- c(xesti,yk)
  ykernel <- rbind(ykernel,xkyk)
}
plot(x,y,xlab = "Time", ylab = "Number of Total Strains", col = 'blue', cex = 2)
lines(ykernel[,1],ykernel[,2], col = 'red', lwd = 2)


b <- 5 #bandwidth
kdeEstimateyX <- seq(0,510,1)
ykernel <- NULL
for(xesti in kdeEstimateyX){
  xx <-  xesti - x
  K <-gausinKernel(xx,b)
  Ksum <- sum(K)
  weight <- K/Ksum
  yk <- sum(weight*y)
  xkyk <- c(xesti,yk)
  ykernel <- rbind(ykernel,xkyk)
}
plot(x,y,xlab = "Time", ylab = "Number of Total Strains", col = 'blue', cex = 2)
lines(ykernel[,1],ykernel[,2], col = 'red', lwd = 2)


b <- 15 #bandwidth
kdeEstimateyX <- seq(0,510,1)
ykernel <- NULL
for(xesti in kdeEstimateyX){
  xx <-  xesti - x
  K <-gausinKernel(xx,b)
  Ksum <- sum(K)
  weight <- K/Ksum
  yk <- sum(weight*y)
  xkyk <- c(xesti,yk)
  ykernel <- rbind(ykernel,xkyk)
}
plot(x,y,xlab = "Time", ylab = "Number of Total Strains", col = 'blue', cex = 2)
lines(ykernel[,1],ykernel[,2], col = 'red', lwd = 2)



lm1 <- lm(ykernel[,2] ~ ykernel[,1])


coef(lm1)


# check R-squared values
summary(lm1)$r.squared

# ------------------------------------------------------------------------------
# -------------------KSmoothening for Total severity----------------------------
# ------------------------------------------------------------------------------



x <- df$x
y <- df$Total_severity




b <- 10 #bandwidth
kdeEstimateyX <- seq(0,510,1)
ykernel <- NULL
for(xesti in kdeEstimateyX){
  xx <-  xesti - x
  K <-gausinKernel(xx,b)
  Ksum <- sum(K)
  weight <- K/Ksum
  yk <- sum(weight*y)
  xkyk <- c(xesti,yk)
  ykernel <- rbind(ykernel,xkyk)
}
plot(x,y,xlab = "Time", ylab = "Severity", col = 'blue', cex = 2)
lines(ykernel[,1],ykernel[,2], col = 'red', lwd = 2)


b <- 5 #bandwidth
kdeEstimateyX <- seq(0,510,1)
ykernel <- NULL
for(xesti in kdeEstimateyX){
  xx <-  xesti - x
  K <-gausinKernel(xx,b)
  Ksum <- sum(K)
  weight <- K/Ksum
  yk <- sum(weight*y)
  xkyk <- c(xesti,yk)
  ykernel <- rbind(ykernel,xkyk)
}
plot(x,y,xlab = "Time", ylab = "Severity", col = 'blue', cex = 2)
lines(ykernel[,1],ykernel[,2], col = 'red', lwd = 2)


b <- 15 #bandwidth
kdeEstimateyX <- seq(0,510,1)
ykernel <- NULL
for(xesti in kdeEstimateyX){
  xx <-  xesti - x
  K <-gausinKernel(xx,b)
  Ksum <- sum(K)
  weight <- K/Ksum
  yk <- sum(weight*y)
  xkyk <- c(xesti,yk)
  ykernel <- rbind(ykernel,xkyk)
}
plot(x,y,xlab = "Time", ylab = "Severity", col = 'blue', cex = 2)
lines(ykernel[,1],ykernel[,2], col = 'red', lwd = 2)



lm1 <- lm(ykernel[,2] ~ ykernel[,1])


coef(lm1)


# check R-squared values
summary(lm1)$r.squared




par(mar = c(5, 4, 4, 4) + 0.3)              # Additional space for second y-axis
plot(df$x, df$total_cases, pch = 16, col = 2)              # Create first plot
par(new = TRUE)                             # Add new plot
plot(df$x, df$total_deaths, pch = 17, col = 3,              # Create second plot without axes
     axes = FALSE, xlab = "", ylab = "")
axis(side = 4, at = pretty(range(df$total_deaths)))      # Add second axis
mtext("total_deaths", side = 4, line = 3)             # Add second axis label