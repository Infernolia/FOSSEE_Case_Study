rm(list=ls())

# Need the "tseries" package for "adf.test" function.
#install.packages("tseries")
library(tseries)
#---------------------------------------------------------------------------------------
# Polynomial Curve Fitting
#---------------------------------------------------------------------------------------

df <- read.csv("data_final/combined_final_data.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))


df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}


df$y <- df$total_cases


fit1 <- lm(y~x, data=df)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=df)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=df)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=df)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=df)
fit6 <- lm(y~poly(x,6,raw=TRUE), data=df)
fit7 <- lm(y~poly(x,7,raw=TRUE), data=df)
fit8 <- lm(y~poly(x,8,raw=TRUE), data=df)
fit9 <- lm(y~poly(x,9,raw=TRUE), data=df)
fit10 <- lm(y~poly(x,10,raw=TRUE), data=df)
fit11 <- lm(y~poly(x,11,raw=TRUE), data=df)
fit12 <- lm(y~poly(x,12,raw=TRUE), data=df)
fit13 <- lm(y~poly(x,13,raw=TRUE), data=df)
fit25 <- lm(y~poly(x,25,raw=TRUE), data=df)
fit35 <- lm(y~poly(x,35,raw=TRUE), data=df)
fit100 <- lm(y~poly(x,100,raw=TRUE), data=df)

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='Time', ylab='Total Cases')

#define x-axis values
x_axis <- seq(1, 436, length=436)

#add curve of each model to plot
lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green')
lines(x_axis, predict(fit2, data.frame(x=x_axis)), col='red')
lines(x_axis, predict(fit3, data.frame(x=x_axis)), col='purple')
lines(x_axis, predict(fit4, data.frame(x=x_axis)), col='blue')
lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='orange')
lines(x_axis, predict(fit6, data.frame(x=x_axis)), col='black')
lines(x_axis, predict(fit7, data.frame(x=x_axis)), col='grey')
lines(x_axis, predict(fit8, data.frame(x=x_axis)), col='chartreuse3')
lines(x_axis, predict(fit9, data.frame(x=x_axis)), col='cornflowerblue')
lines(x_axis, predict(fit10, data.frame(x=x_axis)), col='darkgoldenrod1')
lines(x_axis, predict(fit11, data.frame(x=x_axis)), col='peachpuff3')
lines(x_axis, predict(fit12, data.frame(x=x_axis)), col='mediumorchid2')
lines(x_axis, predict(fit13, data.frame(x=x_axis)), col='turquoise3')
lines(x_axis, predict(fit25, data.frame(x=x_axis)), col='wheat4')
lines(x_axis, predict(fit35, data.frame(x=x_axis)), col='cyan')
lines(x_axis, predict(fit100, data.frame(x=x_axis)), col='magenta')
legend("topleft", c("Degree 1","Degree 2","Degree 3","Degree 4","Degree 5","Degree 6","Degree 7","Degree 8","Degree 9","Degree 10","Degree 11","Degree 12","Degree 13","Degree 25","Degree 35","Degree 100"),lty = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), lwd = 2, col = c('green','red','purple','blue','orange','black','grey','chartreuse3','cornflowerblue','darkgoldenrod1','peachpuff3','mediumorchid2','turquoise3','wheat4','cyan','magenta'), bty = "n")

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared
summary(fit6)$adj.r.squared
summary(fit7)$adj.r.squared
summary(fit8)$adj.r.squared
summary(fit9)$adj.r.squared
summary(fit10)$adj.r.squared
summary(fit11)$adj.r.squared
summary(fit12)$adj.r.squared
summary(fit13)$adj.r.squared
summary(fit25)$adj.r.squared
summary(fit35)$adj.r.squared
summary(fit100)$adj.r.squared

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='Time', ylab='Total Cases')

#define x-axis values
x_axis <- seq(1, 436, length=436)

#add curve of fourth-degree polynomial model
lines(x_axis, predict(fit25, data.frame(x=x_axis)), col='blue')


summary(fit25)

plot(fit25$residuals,ylab="Total Cases Residuals")

#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------

rm(list=ls())

df <- read.csv("data_final/combined_final_data.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))


#Finding the curve that fits best to the total_deaths trend.

df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}


df$y <- df$total_deaths


fit1 <- lm(y~x, data=df)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=df)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=df)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=df)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=df)
fit6 <- lm(y~poly(x,6,raw=TRUE), data=df)
fit7 <- lm(y~poly(x,7,raw=TRUE), data=df)
fit8 <- lm(y~poly(x,8,raw=TRUE), data=df)
fit9 <- lm(y~poly(x,9,raw=TRUE), data=df)
fit10 <- lm(y~poly(x,10,raw=TRUE), data=df)
fit11 <- lm(y~poly(x,11,raw=TRUE), data=df)
fit12 <- lm(y~poly(x,12,raw=TRUE), data=df)
fit13 <- lm(y~poly(x,13,raw=TRUE), data=df)

fit25 <- lm(y~poly(x,25,raw=TRUE), data=df)
fit35 <- lm(y~poly(x,35,raw=TRUE), data=df)
fit100 <- lm(y~poly(x,100,raw=TRUE), data=df)

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='Time', ylab='Total Deaths')

#define x-axis values
x_axis <- seq(1, 436, length=436)

#add curve of each model to plot
lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green')
lines(x_axis, predict(fit2, data.frame(x=x_axis)), col='red')
lines(x_axis, predict(fit3, data.frame(x=x_axis)), col='purple')
lines(x_axis, predict(fit4, data.frame(x=x_axis)), col='blue')
lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='orange')
lines(x_axis, predict(fit6, data.frame(x=x_axis)), col='black')
lines(x_axis, predict(fit7, data.frame(x=x_axis)), col='grey')
lines(x_axis, predict(fit8, data.frame(x=x_axis)), col='chartreuse3')
lines(x_axis, predict(fit9, data.frame(x=x_axis)), col='cornflowerblue')
lines(x_axis, predict(fit10, data.frame(x=x_axis)), col='darkgoldenrod1')
lines(x_axis, predict(fit11, data.frame(x=x_axis)), col='peachpuff3')
lines(x_axis, predict(fit12, data.frame(x=x_axis)), col='mediumorchid2')
lines(x_axis, predict(fit13, data.frame(x=x_axis)), col='turquoise3')
lines(x_axis, predict(fit25, data.frame(x=x_axis)), col='wheat4')
lines(x_axis, predict(fit35, data.frame(x=x_axis)), col='cyan')
lines(x_axis, predict(fit100, data.frame(x=x_axis)), col='magenta')
legend("topleft", c("Degree 1","Degree 2","Degree 3","Degree 4","Degree 5","Degree 6","Degree 7","Degree 8","Degree 9","Degree 10","Degree 11","Degree 12","Degree 13","Degree 25","Degree 35","Degree 100"),lty = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), lwd = 2, col = c('green','red','purple','blue','orange','black','grey','chartreuse3','cornflowerblue','darkgoldenrod1','peachpuff3','mediumorchid2','turquoise3','wheat4','cyan','magenta'), bty = "n")


summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared
summary(fit6)$adj.r.squared
summary(fit7)$adj.r.squared
summary(fit8)$adj.r.squared
summary(fit9)$adj.r.squared
summary(fit10)$adj.r.squared
summary(fit11)$adj.r.squared
summary(fit12)$adj.r.squared
summary(fit13)$adj.r.squared
summary(fit25)$adj.r.squared
summary(fit35)$adj.r.squared
summary(fit100)$adj.r.squared

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='Time', ylab='Total Deaths')

#define x-axis values
x_axis <- seq(1, 436, length=436)

#add curve of fourth-degree polynomial model
lines(x_axis, predict(fit11, data.frame(x=x_axis)), col='blue')


summary(fit11)

plot(fit11$residuals,ylab="Total Deaths Residuals")

#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------

rm(list=ls())

df <- read.csv("data_final/combined_final_data.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))


#Finding the curve that fits best to the total_strains trend.

df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}


df$y <- df$total_strains


fit1 <- lm(y~x, data=df)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=df)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=df)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=df)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=df)
fit6 <- lm(y~poly(x,6,raw=TRUE), data=df)
fit7 <- lm(y~poly(x,7,raw=TRUE), data=df)
fit8 <- lm(y~poly(x,8,raw=TRUE), data=df)
fit9 <- lm(y~poly(x,9,raw=TRUE), data=df)
fit10 <- lm(y~poly(x,10,raw=TRUE), data=df)
fit11 <- lm(y~poly(x,11,raw=TRUE), data=df)
fit12 <- lm(y~poly(x,12,raw=TRUE), data=df)
fit13 <- lm(y~poly(x,13,raw=TRUE), data=df)

fit25 <- lm(y~poly(x,25,raw=TRUE), data=df)
fit35 <- lm(y~poly(x,35,raw=TRUE), data=df)
fit100 <- lm(y~poly(x,100,raw=TRUE), data=df)

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='Time', ylab='Total Strains')

#define x-axis values
x_axis <- seq(1, 436, length=436)

#add curve of each model to plot
lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green')
lines(x_axis, predict(fit2, data.frame(x=x_axis)), col='red')
lines(x_axis, predict(fit3, data.frame(x=x_axis)), col='purple')
lines(x_axis, predict(fit4, data.frame(x=x_axis)), col='blue')
lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='orange')
lines(x_axis, predict(fit6, data.frame(x=x_axis)), col='black')
lines(x_axis, predict(fit7, data.frame(x=x_axis)), col='grey')
lines(x_axis, predict(fit8, data.frame(x=x_axis)), col='chartreuse3')
lines(x_axis, predict(fit9, data.frame(x=x_axis)), col='cornflowerblue')
lines(x_axis, predict(fit10, data.frame(x=x_axis)), col='darkgoldenrod1')
lines(x_axis, predict(fit11, data.frame(x=x_axis)), col='peachpuff3')
lines(x_axis, predict(fit12, data.frame(x=x_axis)), col='mediumorchid2')
lines(x_axis, predict(fit13, data.frame(x=x_axis)), col='turquoise3')
lines(x_axis, predict(fit25, data.frame(x=x_axis)), col='wheat4')
lines(x_axis, predict(fit35, data.frame(x=x_axis)), col='cyan')
lines(x_axis, predict(fit100, data.frame(x=x_axis)), col='magenta')
legend("topleft", c("Degree 1","Degree 2","Degree 3","Degree 4","Degree 5","Degree 6","Degree 7","Degree 8","Degree 9","Degree 10","Degree 11","Degree 12","Degree 13","Degree 25","Degree 35","Degree 100"),lty = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), lwd = 2, col = c('green','red','purple','blue','orange','black','grey','chartreuse3','cornflowerblue','darkgoldenrod1','peachpuff3','mediumorchid2','turquoise3','wheat4','cyan','magenta'), bty = "n")

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared
summary(fit6)$adj.r.squared
summary(fit7)$adj.r.squared
summary(fit8)$adj.r.squared
summary(fit9)$adj.r.squared
summary(fit10)$adj.r.squared
summary(fit11)$adj.r.squared
summary(fit12)$adj.r.squared
summary(fit13)$adj.r.squared
summary(fit25)$adj.r.squared
summary(fit35)$adj.r.squared
summary(fit100)$adj.r.squared

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='Time', ylab='Total Strains')

#define x-axis values
x_axis <- seq(1, 436, length=436)

#add curve of fourth-degree polynomial model
lines(x_axis, predict(fit25, data.frame(x=x_axis)), col='blue')


summary(fit25)

plot(fit25$residuals,ylab="Total Strains Residuals")


#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------

rm(list=ls())

df <- read.csv("data_final/combined_final_data.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))


#Finding the curve that fits best to the Total_severity trend.

df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}


df$y <- df$Total_severity


fit1 <- lm(y~x, data=df)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=df)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=df)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=df)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=df)
fit6 <- lm(y~poly(x,6,raw=TRUE), data=df)
fit7 <- lm(y~poly(x,7,raw=TRUE), data=df)
fit8 <- lm(y~poly(x,8,raw=TRUE), data=df)
fit9 <- lm(y~poly(x,9,raw=TRUE), data=df)
fit10 <- lm(y~poly(x,10,raw=TRUE), data=df)
fit11 <- lm(y~poly(x,11,raw=TRUE), data=df)
fit12 <- lm(y~poly(x,12,raw=TRUE), data=df)
fit13 <- lm(y~poly(x,13,raw=TRUE), data=df)
fit25 <- lm(y~poly(x,25,raw=TRUE), data=df)
fit35 <- lm(y~poly(x,35,raw=TRUE), data=df)
fit100 <- lm(y~poly(x,100,raw=TRUE), data=df)

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='Time', ylab='Total Severity')

#define x-axis values
x_axis <- seq(1, 436, length=436)

#add curve of each model to plot
lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green')
lines(x_axis, predict(fit2, data.frame(x=x_axis)), col='red')
lines(x_axis, predict(fit3, data.frame(x=x_axis)), col='purple')
lines(x_axis, predict(fit4, data.frame(x=x_axis)), col='blue')
lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='orange')
lines(x_axis, predict(fit6, data.frame(x=x_axis)), col='black')
lines(x_axis, predict(fit7, data.frame(x=x_axis)), col='grey')
lines(x_axis, predict(fit8, data.frame(x=x_axis)), col='chartreuse3')
lines(x_axis, predict(fit9, data.frame(x=x_axis)), col='cornflowerblue')
lines(x_axis, predict(fit10, data.frame(x=x_axis)), col='darkgoldenrod1')
lines(x_axis, predict(fit11, data.frame(x=x_axis)), col='peachpuff3')
lines(x_axis, predict(fit12, data.frame(x=x_axis)), col='mediumorchid2')
lines(x_axis, predict(fit13, data.frame(x=x_axis)), col='turquoise3')
lines(x_axis, predict(fit25, data.frame(x=x_axis)), col='wheat4')
lines(x_axis, predict(fit35, data.frame(x=x_axis)), col='cyan')
lines(x_axis, predict(fit100, data.frame(x=x_axis)), col='magenta')
legend("topleft", c("Degree 1","Degree 2","Degree 3","Degree 4","Degree 5","Degree 6","Degree 7","Degree 8","Degree 9","Degree 10","Degree 11","Degree 12","Degree 13","Degree 25","Degree 35","Degree 100"),lty = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), lwd = 2, col = c('green','red','purple','blue','orange','black','grey','chartreuse3','cornflowerblue','darkgoldenrod1','peachpuff3','mediumorchid2','turquoise3','wheat4','cyan','magenta'), bty = "n")

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared
summary(fit6)$adj.r.squared
summary(fit7)$adj.r.squared
summary(fit8)$adj.r.squared
summary(fit9)$adj.r.squared
summary(fit10)$adj.r.squared
summary(fit11)$adj.r.squared
summary(fit12)$adj.r.squared
summary(fit13)$adj.r.squared
summary(fit25)$adj.r.squared
summary(fit35)$adj.r.squared
summary(fit100)$adj.r.squared

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='Time', ylab='Total Severity')

#define x-axis values
x_axis <- seq(1, 436, length=436)

#add curve of fourth-degree polynomial model
lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='blue')


summary(fit5)

plot(fit5$residuals,ylab="Total Severity Residuals")






#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------

rm(list=ls())

df <- read.csv("data_final/combined_final_data.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))


#Finding the curve that fits best to the Total_severity trend.

df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}


df$y <- df$alpha_strains


fit1 <- lm(y~x, data=df)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=df)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=df)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=df)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=df)
fit6 <- lm(y~poly(x,6,raw=TRUE), data=df)
fit7 <- lm(y~poly(x,7,raw=TRUE), data=df)
fit8 <- lm(y~poly(x,8,raw=TRUE), data=df)
fit9 <- lm(y~poly(x,9,raw=TRUE), data=df)
fit10 <- lm(y~poly(x,10,raw=TRUE), data=df)
fit11 <- lm(y~poly(x,11,raw=TRUE), data=df)
fit12 <- lm(y~poly(x,12,raw=TRUE), data=df)
fit13 <- lm(y~poly(x,13,raw=TRUE), data=df)
fit25 <- lm(y~poly(x,25,raw=TRUE), data=df)
fit35 <- lm(y~poly(x,35,raw=TRUE), data=df)
fit100 <- lm(y~poly(x,100,raw=TRUE), data=df)

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='Time', ylab='Alpha Strains')

#define x-axis values
x_axis <- seq(1, 436, length=436)

#add curve of each model to plot
lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green')
lines(x_axis, predict(fit2, data.frame(x=x_axis)), col='red')
lines(x_axis, predict(fit3, data.frame(x=x_axis)), col='purple')
lines(x_axis, predict(fit4, data.frame(x=x_axis)), col='blue')
lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='orange')
lines(x_axis, predict(fit6, data.frame(x=x_axis)), col='black')
lines(x_axis, predict(fit7, data.frame(x=x_axis)), col='grey')
lines(x_axis, predict(fit8, data.frame(x=x_axis)), col='chartreuse3')
lines(x_axis, predict(fit9, data.frame(x=x_axis)), col='cornflowerblue')
lines(x_axis, predict(fit10, data.frame(x=x_axis)), col='darkgoldenrod1')
lines(x_axis, predict(fit11, data.frame(x=x_axis)), col='peachpuff3')
lines(x_axis, predict(fit12, data.frame(x=x_axis)), col='mediumorchid2')
lines(x_axis, predict(fit13, data.frame(x=x_axis)), col='turquoise3')
lines(x_axis, predict(fit25, data.frame(x=x_axis)), col='wheat4')
lines(x_axis, predict(fit35, data.frame(x=x_axis)), col='cyan')
lines(x_axis, predict(fit100, data.frame(x=x_axis)), col='magenta')
legend("topleft", c("Degree 1","Degree 2","Degree 3","Degree 4","Degree 5","Degree 6","Degree 7","Degree 8","Degree 9","Degree 10","Degree 11","Degree 12","Degree 13","Degree 25","Degree 35","Degree 100"),lty = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), lwd = 2, col = c('green','red','purple','blue','orange','black','grey','chartreuse3','cornflowerblue','darkgoldenrod1','peachpuff3','mediumorchid2','turquoise3','wheat4','cyan','magenta'), bty = "n")

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared
summary(fit6)$adj.r.squared
summary(fit7)$adj.r.squared
summary(fit8)$adj.r.squared
summary(fit9)$adj.r.squared
summary(fit10)$adj.r.squared
summary(fit11)$adj.r.squared
summary(fit12)$adj.r.squared
summary(fit13)$adj.r.squared
summary(fit25)$adj.r.squared
summary(fit35)$adj.r.squared
summary(fit100)$adj.r.squared

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='Time', ylab='Alpha Strains')

#define x-axis values
x_axis <- seq(1, 436, length=436)

#add curve of fourth-degree polynomial model
lines(x_axis, predict(fit25, data.frame(x=x_axis)), col='blue')


summary(fit25)

plot(fit25$residuals,ylab="Alpha Strains Residuals")



rm(list=ls())

#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------

rm(list=ls())

df <- read.csv("data_final/combined_final_data.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))


#Finding the curve that fits best to the Total_severity trend.

df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}


df$y <- df$beta_strains


fit1 <- lm(y~x, data=df)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=df)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=df)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=df)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=df)
fit6 <- lm(y~poly(x,6,raw=TRUE), data=df)
fit7 <- lm(y~poly(x,7,raw=TRUE), data=df)
fit8 <- lm(y~poly(x,8,raw=TRUE), data=df)
fit9 <- lm(y~poly(x,9,raw=TRUE), data=df)
fit10 <- lm(y~poly(x,10,raw=TRUE), data=df)
fit11 <- lm(y~poly(x,11,raw=TRUE), data=df)
fit12 <- lm(y~poly(x,12,raw=TRUE), data=df)
fit13 <- lm(y~poly(x,13,raw=TRUE), data=df)
fit25 <- lm(y~poly(x,25,raw=TRUE), data=df)
fit35 <- lm(y~poly(x,35,raw=TRUE), data=df)
fit100 <- lm(y~poly(x,100,raw=TRUE), data=df)

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='Time', ylab='Beta Strains')

#define x-axis values
x_axis <- seq(1, 436, length=436)

#add curve of each model to plot
lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green')
lines(x_axis, predict(fit2, data.frame(x=x_axis)), col='red')
lines(x_axis, predict(fit3, data.frame(x=x_axis)), col='purple')
lines(x_axis, predict(fit4, data.frame(x=x_axis)), col='blue')
lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='orange')
lines(x_axis, predict(fit6, data.frame(x=x_axis)), col='black')
lines(x_axis, predict(fit7, data.frame(x=x_axis)), col='grey')
lines(x_axis, predict(fit8, data.frame(x=x_axis)), col='chartreuse3')
lines(x_axis, predict(fit9, data.frame(x=x_axis)), col='cornflowerblue')
lines(x_axis, predict(fit10, data.frame(x=x_axis)), col='darkgoldenrod1')
lines(x_axis, predict(fit11, data.frame(x=x_axis)), col='peachpuff3')
lines(x_axis, predict(fit12, data.frame(x=x_axis)), col='mediumorchid2')
lines(x_axis, predict(fit13, data.frame(x=x_axis)), col='turquoise3')
lines(x_axis, predict(fit25, data.frame(x=x_axis)), col='wheat4')
lines(x_axis, predict(fit35, data.frame(x=x_axis)), col='cyan')
lines(x_axis, predict(fit100, data.frame(x=x_axis)), col='magenta')
legend("topleft", c("Degree 1","Degree 2","Degree 3","Degree 4","Degree 5","Degree 6","Degree 7","Degree 8","Degree 9","Degree 10","Degree 11","Degree 12","Degree 13","Degree 25","Degree 35","Degree 100"),lty = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), lwd = 2, col = c('green','red','purple','blue','orange','black','grey','chartreuse3','cornflowerblue','darkgoldenrod1','peachpuff3','mediumorchid2','turquoise3','wheat4','cyan','magenta'), bty = "n")

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared
summary(fit6)$adj.r.squared
summary(fit7)$adj.r.squared
summary(fit8)$adj.r.squared
summary(fit9)$adj.r.squared
summary(fit10)$adj.r.squared
summary(fit11)$adj.r.squared
summary(fit12)$adj.r.squared
summary(fit13)$adj.r.squared
summary(fit25)$adj.r.squared
summary(fit35)$adj.r.squared
summary(fit100)$adj.r.squared

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='Time', ylab='Beta Strains')

#define x-axis values
x_axis <- seq(1, 436, length=436)

#add curve of fourth-degree polynomial model
lines(x_axis, predict(fit9, data.frame(x=x_axis)), col='blue')


summary(fit9)
plot(fit9$residuals,ylab="Beta Strains Residuals")



rm(list=ls())


#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------

rm(list=ls())

df <- read.csv("data_final/combined_final_data.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))


#Finding the curve that fits best to the Total_severity trend.

df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}


df$y <- df$gamma_strains


fit1 <- lm(y~x, data=df)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=df)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=df)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=df)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=df)
fit6 <- lm(y~poly(x,6,raw=TRUE), data=df)
fit7 <- lm(y~poly(x,7,raw=TRUE), data=df)
fit8 <- lm(y~poly(x,8,raw=TRUE), data=df)
fit9 <- lm(y~poly(x,9,raw=TRUE), data=df)
fit10 <- lm(y~poly(x,10,raw=TRUE), data=df)
fit11 <- lm(y~poly(x,11,raw=TRUE), data=df)
fit12 <- lm(y~poly(x,12,raw=TRUE), data=df)
fit13 <- lm(y~poly(x,13,raw=TRUE), data=df)
fit25 <- lm(y~poly(x,25,raw=TRUE), data=df)
fit35 <- lm(y~poly(x,35,raw=TRUE), data=df)
fit100 <- lm(y~poly(x,100,raw=TRUE), data=df)

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='Time', ylab='Gamma Strains')

#define x-axis values
x_axis <- seq(1, 436, length=436)

#add curve of each model to plot
lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green')
lines(x_axis, predict(fit2, data.frame(x=x_axis)), col='red')
lines(x_axis, predict(fit3, data.frame(x=x_axis)), col='purple')
lines(x_axis, predict(fit4, data.frame(x=x_axis)), col='blue')
lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='orange')
lines(x_axis, predict(fit6, data.frame(x=x_axis)), col='black')
lines(x_axis, predict(fit7, data.frame(x=x_axis)), col='grey')
lines(x_axis, predict(fit8, data.frame(x=x_axis)), col='chartreuse3')
lines(x_axis, predict(fit9, data.frame(x=x_axis)), col='cornflowerblue')
lines(x_axis, predict(fit10, data.frame(x=x_axis)), col='darkgoldenrod1')
lines(x_axis, predict(fit11, data.frame(x=x_axis)), col='peachpuff3')
lines(x_axis, predict(fit12, data.frame(x=x_axis)), col='mediumorchid2')
lines(x_axis, predict(fit13, data.frame(x=x_axis)), col='turquoise3')
lines(x_axis, predict(fit25, data.frame(x=x_axis)), col='wheat4')
lines(x_axis, predict(fit35, data.frame(x=x_axis)), col='cyan')
lines(x_axis, predict(fit100, data.frame(x=x_axis)), col='magenta')
legend("topleft", c("Degree 1","Degree 2","Degree 3","Degree 4","Degree 5","Degree 6","Degree 7","Degree 8","Degree 9","Degree 10","Degree 11","Degree 12","Degree 13","Degree 25","Degree 35","Degree 100"),lty = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), lwd = 2, col = c('green','red','purple','blue','orange','black','grey','chartreuse3','cornflowerblue','darkgoldenrod1','peachpuff3','mediumorchid2','turquoise3','wheat4','cyan','magenta'), bty = "n")

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared
summary(fit6)$adj.r.squared
summary(fit7)$adj.r.squared
summary(fit8)$adj.r.squared
summary(fit9)$adj.r.squared
summary(fit10)$adj.r.squared
summary(fit11)$adj.r.squared
summary(fit12)$adj.r.squared
summary(fit13)$adj.r.squared
summary(fit25)$adj.r.squared
summary(fit35)$adj.r.squared
summary(fit100)$adj.r.squared

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='Time', ylab='Gamma Strains')

#define x-axis values
x_axis <- seq(1, 436, length=436)

#add curve of fourth-degree polynomial model
lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='blue')


summary(fit5)
plot(fit5$residuals,ylab="Gamma Strains Residuals")



rm(list=ls())


#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------

rm(list=ls())

df <- read.csv("data_final/combined_final_data.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))


#Finding the curve that fits best to the Total_severity trend.

df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}


df$y <- df$delta_strains


fit1 <- lm(y~x, data=df)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=df)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=df)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=df)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=df)
fit6 <- lm(y~poly(x,6,raw=TRUE), data=df)
fit7 <- lm(y~poly(x,7,raw=TRUE), data=df)
fit8 <- lm(y~poly(x,8,raw=TRUE), data=df)
fit9 <- lm(y~poly(x,9,raw=TRUE), data=df)
fit10 <- lm(y~poly(x,10,raw=TRUE), data=df)
fit11 <- lm(y~poly(x,11,raw=TRUE), data=df)
fit12 <- lm(y~poly(x,12,raw=TRUE), data=df)
fit13 <- lm(y~poly(x,13,raw=TRUE), data=df)
fit25 <- lm(y~poly(x,25,raw=TRUE), data=df)
fit35 <- lm(y~poly(x,35,raw=TRUE), data=df)
fit100 <- lm(y~poly(x,100,raw=TRUE), data=df)

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='Time', ylab='Delta Strains')

#define x-axis values
x_axis <- seq(1, 436, length=436)

#add curve of each model to plot
lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green')
lines(x_axis, predict(fit2, data.frame(x=x_axis)), col='red')
lines(x_axis, predict(fit3, data.frame(x=x_axis)), col='purple')
lines(x_axis, predict(fit4, data.frame(x=x_axis)), col='blue')
lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='orange')
lines(x_axis, predict(fit6, data.frame(x=x_axis)), col='black')
lines(x_axis, predict(fit7, data.frame(x=x_axis)), col='grey')
lines(x_axis, predict(fit8, data.frame(x=x_axis)), col='chartreuse3')
lines(x_axis, predict(fit9, data.frame(x=x_axis)), col='cornflowerblue')
lines(x_axis, predict(fit10, data.frame(x=x_axis)), col='darkgoldenrod1')
lines(x_axis, predict(fit11, data.frame(x=x_axis)), col='peachpuff3')
lines(x_axis, predict(fit12, data.frame(x=x_axis)), col='mediumorchid2')
lines(x_axis, predict(fit13, data.frame(x=x_axis)), col='turquoise3')
lines(x_axis, predict(fit25, data.frame(x=x_axis)), col='wheat4')
lines(x_axis, predict(fit35, data.frame(x=x_axis)), col='cyan')
lines(x_axis, predict(fit100, data.frame(x=x_axis)), col='magenta')
legend("topleft", c("Degree 1","Degree 2","Degree 3","Degree 4","Degree 5","Degree 6","Degree 7","Degree 8","Degree 9","Degree 10","Degree 11","Degree 12","Degree 13","Degree 25","Degree 35","Degree 100"),lty = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), lwd = 2, col = c('green','red','purple','blue','orange','black','grey','chartreuse3','cornflowerblue','darkgoldenrod1','peachpuff3','mediumorchid2','turquoise3','wheat4','cyan','magenta'), bty = "n")

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared
summary(fit6)$adj.r.squared
summary(fit7)$adj.r.squared
summary(fit8)$adj.r.squared
summary(fit9)$adj.r.squared
summary(fit10)$adj.r.squared
summary(fit11)$adj.r.squared
summary(fit12)$adj.r.squared
summary(fit13)$adj.r.squared
summary(fit25)$adj.r.squared
summary(fit35)$adj.r.squared
summary(fit100)$adj.r.squared

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='Time', ylab='Delta Strains')

#define x-axis values
x_axis <- seq(1, 436, length=436)

#add curve of fourth-degree polynomial model
lines(x_axis, predict(fit9, data.frame(x=x_axis)), col='blue')


summary(fit9)
plot(fit9$residuals,ylab=" Delta Strains Residuals")



rm(list=ls())








#-----------------------------------------------------------------------------------
# Stationarity tests
#-----------------------------------------------------------------------------------
rm(list=ls())
df <- read.csv("data_final/combined_final_data.csv") # Reading the number of cases
df <- subset(df,select = -c(1))

t1 <- df$total_cases
t2 <- df$total_deaths
t3 <- df$total_strains
t4 <- df$Total_severity
t5 <- df$alpha_strains
t6 <- df$beta_strains
t7 <- df$gamma_strains
t8 <- df$delta_strains

adf1 = adf.test(t1)
adf1

adf2 = adf.test(t2)
adf2

adf3 = adf.test(t3)
adf3

adf4 = adf.test(t4)
adf4

adf5 = adf.test(t5)
adf5

adf6 = adf.test(t6)
adf6

adf7 = adf.test(t7)
adf7

adf8 = adf.test(t8)
adf8




#-----------------------------------------------------------------------------------
# Smoothening curves with Kernel Regression with validation and comparing with other regressions
#-----------------------------------------------------------------------------------



loess.gcv <- function(x, y){
  nobs <- length(y)
  xs <- sort(x, index.return = TRUE)
  x <- xs$x
  y <- y[xs$ix]
  tune.loess <- function(s){
    lo <- loess(y ~ x, span = s)
    mean((lo$fitted - y)^2) / (1 - lo$trace.hat/nobs)^2
  }
  os <- optimize(tune.loess, interval = c(.01, 99))$minimum
  lo <- loess(y ~ x, span = os)
  list(x = x, y = lo$fitted, df = lo$trace.hat, span = os)
}


ksmooth.gcv <- function(x, y){
  nobs <- length(y)
  xs <- sort(x, index.return = TRUE)
  x <- xs$x
  y <- y[xs$ix]
  xdif <- outer(x, x, FUN = "-")
  tune.ksmooth <- function(h){
    xden <- dnorm(xdif / h)
    xden <- xden / rowSums(xden)
    df <- sum(diag(xden))
    fit <- xden %*% y
    mean((fit - y)^2) / (1 - df/nobs)^2
  }
  xrng <- diff(range(x))
  oh <- optimize(tune.ksmooth, interval = c(xrng/nobs, xrng))$minimum
  if(any(oh == c(xrng/nobs, xrng)))
    warning("Minimum found on boundary of search range.\nYou should retune model with expanded range.")
  xden <- dnorm(xdif / oh)
  xden <- xden / rowSums(xden)
  df <- sum(diag(xden))
  fit <- xden %*% y
  list(x = x, y = fit, df = df, h = oh)
}




# -------------------------------------------------------------------------------------------------------------------
# Smoothening time series using Local Averaging, Local Regression and Kernel Regression for Total Cases
# -------------------------------------------------------------------------------------------------------------------

df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}
# local averaging (cv span selection)
locavg <- with(df, supsmu(x,total_cases))

# local regression (gcv span selection)
locreg <- with(df, loess.gcv(x,total_cases))
locreg$df


cases <- with(df, ksmooth.gcv(x,total_cases))
cases$df

# plot data
plot(df$x, df$total_cases,xlab = "Time", ylab = "Total Cases")

lines(locavg, lwd = 2)
lines(locreg, lwd = 2, lty = 2, col = "red")
lines(cases, lwd = 2, lty = 3, col = "blue")

legend("topleft", c("supsmu", "loess", "ksmooth"),
       lty = 1:3, lwd = 2, col = c("black", "red", "blue"), bty = "n")

# -------------------------------------------------------------------------------------------------------------------
# Smoothening time series using Local Averaging, Local Regression and Kernel Regression for Total Deaths
# -------------------------------------------------------------------------------------------------------------------
# local averaging (cv span selection)
locavg <- with(df, supsmu(x,total_deaths))

# local regression (gcv span selection)
locreg <- with(df, loess.gcv(x,total_deaths))
locreg$df


deaths <- with(df, ksmooth.gcv(x,total_deaths))
deaths$df

# plot data
plot(df$x, df$total_deaths,xlab = "Time", ylab = "Total Deaths")

lines(locavg, lwd = 2)
lines(locreg, lwd = 2, lty = 2, col = "red")
lines(deaths, lwd = 2, lty = 3, col = "blue")

legend("topleft", c("supsmu", "loess", "ksmooth"),
       lty = 1:3, lwd = 2, col = c("black", "red", "blue"), bty = "n")


# -------------------------------------------------------------------------------------------------------------------
# Smoothening time series using Local Averaging, Local Regression and Kernel Regression for Total Strains
# -------------------------------------------------------------------------------------------------------------------

# local averaging (cv span selection)
locavg <- with(df, supsmu(x,total_strains))

# local regression (gcv span selection)
locreg <- with(df, loess.gcv(x,total_strains))
locreg$df


strains <- with(df, ksmooth.gcv(x,total_strains))
strains$df

# plot data
plot(df$x, df$total_strains,xlab = "Time", ylab = "Total Strains")

lines(locavg, lwd = 2)
lines(locreg, lwd = 2, lty = 2, col = "red")
lines(strains, lwd = 2, lty = 3, col = "blue")

legend("topleft", c("supsmu", "loess", "ksmooth"),
       lty = 1:3, lwd = 2, col = c("black", "red", "blue"), bty = "n")


# -------------------------------------------------------------------------------------------------------------------
# Smoothening time series using Local Averaging, Local Regression and Kernel Regression for Total Severity
# -------------------------------------------------------------------------------------------------------------------



# local averaging (cv span selection)
locavg <- with(df, supsmu(x,Total_severity))

# local regression (gcv span selection)
locreg <- with(df, loess.gcv(x,Total_severity))
locreg$df


severity <- with(df, ksmooth.gcv(x,Total_severity))
severity$df

# plot data
plot(df$x, df$Total_severity,xlab = "Time", ylab = "Total Severity")

lines(locavg, lwd = 2)
lines(locreg, lwd = 2, lty = 2, col = "red")
lines(severity, lwd = 2, lty = 3, col = "blue")

legend("topleft", c("supsmu", "loess", "ksmooth"),
       lty = 1:3, lwd = 2, col = c("black", "red", "blue"), bty = "n")



#-----------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------------------------
# Smoothening time series using Local Averaging, Local Regression and Kernel Regression for Alpha Strains
# -------------------------------------------------------------------------------------------------------------------

# local averaging (cv span selection)
locavg <- with(df, supsmu(x,alpha_strains))

# local regression (gcv span selection)
locreg <- with(df, loess.gcv(x,alpha_strains))
locreg$df


alpha <- with(df, ksmooth.gcv(x,alpha_strains))
alpha$df

# plot data
plot(df$x, df$alpha_strains,xlab = "Time", ylab = "Alpha Strains")

lines(locavg, lwd = 2)
lines(locreg, lwd = 2, lty = 2, col = "red")
lines(alpha, lwd = 2, lty = 3, col = "blue")

legend("topleft", c("supsmu", "loess", "ksmooth"),
       lty = 1:3, lwd = 2, col = c("black", "red", "blue"), bty = "n")

# -------------------------------------------------------------------------------------------------------------------
# Smoothening time series using Local Averaging, Local Regression and Kernel Regression for Beta Strains
# -------------------------------------------------------------------------------------------------------------------
# local averaging (cv span selection)
locavg <- with(df, supsmu(x,beta_strains))

# local regression (gcv span selection)
locreg <- with(df, loess.gcv(x,beta_strains))
locreg$df


beta <- with(df, ksmooth.gcv(x,beta_strains))
beta$df

# plot data
plot(df$x, df$beta_strains,xlab = "Time", ylab = "Beta Strains")

lines(locavg, lwd = 2)
lines(locreg, lwd = 2, lty = 2, col = "red")
lines(beta, lwd = 2, lty = 3, col = "blue")

legend("topleft", c("supsmu", "loess", "ksmooth"),
       lty = 1:3, lwd = 2, col = c("black", "red", "blue"), bty = "n")


# -------------------------------------------------------------------------------------------------------------------
# Smoothening time series using Local Averaging, Local Regression and Kernel Regression for Gamma Strains
# -------------------------------------------------------------------------------------------------------------------

# local averaging (cv span selection)
locavg <- with(df, supsmu(x,gamma_strains))

# local regression (gcv span selection)
locreg <- with(df, loess.gcv(x,gamma_strains))
locreg$df


gamma <- with(df, ksmooth.gcv(x,gamma_strains))
gamma$df

# plot data
plot(df$x, df$gamma_strains,xlab = "Time", ylab = "Gamma Strains")

lines(locavg, lwd = 2)
lines(locreg, lwd = 2, lty = 2, col = "red")
lines(gamma, lwd = 2, lty = 3, col = "blue")

legend("topleft", c("supsmu", "loess", "ksmooth"),
       lty = 1:3, lwd = 2, col = c("black", "red", "blue"), bty = "n")


# -------------------------------------------------------------------------------------------------------------------
# Smoothening time series using Local Averaging, Local Regression and Kernel Regression for Delta Strains
# -------------------------------------------------------------------------------------------------------------------



# local averaging (cv span selection)
locavg <- with(df, supsmu(x,delta_strains))

# local regression (gcv span selection)
locreg <- with(df, loess.gcv(x,delta_strains))
locreg$df


delta <- with(df, ksmooth.gcv(x,delta_strains))
delta$df

# plot data
plot(df$x, df$delta_strains,xlab = "Time", ylab = "Delta Strains")

lines(locavg, lwd = 2)
lines(locreg, lwd = 2, lty = 2, col = "red")
lines(delta, lwd = 2, lty = 3, col = "blue")

legend("topleft", c("supsmu", "loess", "ksmooth"),
       lty = 1:3, lwd = 2, col = c("black", "red", "blue"), bty = "n")







