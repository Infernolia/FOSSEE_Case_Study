rm(list=ls())
library(stringr)
library(ggplot2)
library(DataExplorer)
library(tseries)
library("TTR")



df <- read.csv("all_merged_done.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))
View(df)


basic <-ggplot() + 
  geom_line(aes(x=df$Date,y=df$total_strains,group = 1),color='blue') + 
  geom_line(aes(x=df$Date,y=df$Total_severity,group = 1),color='red') +
  geom_line(aes(x=df$Date,y=df$total_deaths,group = 1),color='green') +
  geom_line(aes(x=df$Date,y=df$total_cases,group = 1),color='purple') +
  ylab('Values')+xlab('date')

basic

#Finding the curve that fits best to the total_cases trend.

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
plot(df$x, df$y, pch=19, xlab='x', ylab='y')

#define x-axis values
x_axis <- seq(1, 500, length=500)

#add curve of each model to plot
lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green')
lines(x_axis, predict(fit2, data.frame(x=x_axis)), col='red')
lines(x_axis, predict(fit3, data.frame(x=x_axis)), col='purple')
lines(x_axis, predict(fit4, data.frame(x=x_axis)), col='blue')
lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='orange')
lines(x_axis, predict(fit6, data.frame(x=x_axis)), col='black')
lines(x_axis, predict(fit7, data.frame(x=x_axis)), col='grey')
lines(x_axis, predict(fit8, data.frame(x=x_axis)), col='orange')
lines(x_axis, predict(fit9, data.frame(x=x_axis)), col='black')
lines(x_axis, predict(fit10, data.frame(x=x_axis)), col='grey')
lines(x_axis, predict(fit11, data.frame(x=x_axis)), col='green')
lines(x_axis, predict(fit12, data.frame(x=x_axis)), col='red')
lines(x_axis, predict(fit13, data.frame(x=x_axis)), col='purple')
lines(x_axis, predict(fit25, data.frame(x=x_axis)), col='black')
lines(x_axis, predict(fit35, data.frame(x=x_axis)), col='grey')
lines(x_axis, predict(fit100, data.frame(x=x_axis)), col='grey')

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
plot(df$x, df$y, pch=19, xlab='x', ylab='y')

#define x-axis values
x_axis <- seq(1, 500, length=500)

#add curve of fourth-degree polynomial model
lines(x_axis, predict(fit25, data.frame(x=x_axis)), col='blue')


summary(fit25)

#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------

rm(list=ls())

df <- read.csv("all_merged_done.csv")  # Reading the number of cases
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
plot(df$x, df$y, pch=19, xlab='x', ylab='y')

#define x-axis values
x_axis <- seq(1, 500, length=500)

#add curve of each model to plot
lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green')
lines(x_axis, predict(fit2, data.frame(x=x_axis)), col='red')
lines(x_axis, predict(fit3, data.frame(x=x_axis)), col='purple')
lines(x_axis, predict(fit4, data.frame(x=x_axis)), col='blue')
lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='orange')
lines(x_axis, predict(fit6, data.frame(x=x_axis)), col='black')
lines(x_axis, predict(fit7, data.frame(x=x_axis)), col='grey')
lines(x_axis, predict(fit8, data.frame(x=x_axis)), col='orange')
lines(x_axis, predict(fit9, data.frame(x=x_axis)), col='black')
lines(x_axis, predict(fit10, data.frame(x=x_axis)), col='grey')
lines(x_axis, predict(fit11, data.frame(x=x_axis)), col='green')
lines(x_axis, predict(fit12, data.frame(x=x_axis)), col='red')
lines(x_axis, predict(fit13, data.frame(x=x_axis)), col='purple')
lines(x_axis, predict(fit25, data.frame(x=x_axis)), col='black')
lines(x_axis, predict(fit35, data.frame(x=x_axis)), col='grey')
lines(x_axis, predict(fit100, data.frame(x=x_axis)), col='grey')

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
plot(df$x, df$y, pch=19, xlab='x', ylab='y')

#define x-axis values
x_axis <- seq(1, 500, length=500)

#add curve of fourth-degree polynomial model
lines(x_axis, predict(fit25, data.frame(x=x_axis)), col='blue')


summary(fit25)

#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------

rm(list=ls())

df <- read.csv("all_merged_done.csv")  # Reading the number of cases
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
plot(df$x, df$y, pch=19, xlab='x', ylab='y')

#define x-axis values
x_axis <- seq(1, 500, length=500)

#add curve of each model to plot
lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green')
lines(x_axis, predict(fit2, data.frame(x=x_axis)), col='red')
lines(x_axis, predict(fit3, data.frame(x=x_axis)), col='purple')
lines(x_axis, predict(fit4, data.frame(x=x_axis)), col='blue')
lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='orange')
lines(x_axis, predict(fit6, data.frame(x=x_axis)), col='black')
lines(x_axis, predict(fit7, data.frame(x=x_axis)), col='grey')
lines(x_axis, predict(fit8, data.frame(x=x_axis)), col='orange')
lines(x_axis, predict(fit9, data.frame(x=x_axis)), col='black')
lines(x_axis, predict(fit10, data.frame(x=x_axis)), col='grey')
lines(x_axis, predict(fit11, data.frame(x=x_axis)), col='green')
lines(x_axis, predict(fit12, data.frame(x=x_axis)), col='red')
lines(x_axis, predict(fit13, data.frame(x=x_axis)), col='purple')
lines(x_axis, predict(fit25, data.frame(x=x_axis)), col='black')
lines(x_axis, predict(fit35, data.frame(x=x_axis)), col='grey')
lines(x_axis, predict(fit100, data.frame(x=x_axis)), col='grey')

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
plot(df$x, df$y, pch=19, xlab='x', ylab='y')

#define x-axis values
x_axis <- seq(1, 500, length=500)

#add curve of fourth-degree polynomial model
lines(x_axis, predict(fit10, data.frame(x=x_axis)), col='blue')


summary(fit10)

#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------

rm(list=ls())

df <- read.csv("all_merged_done.csv")  # Reading the number of cases
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
plot(df$x, df$y, pch=19, xlab='x', ylab='y')

#define x-axis values
x_axis <- seq(1, 500, length=500)

#add curve of each model to plot
lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green')
lines(x_axis, predict(fit2, data.frame(x=x_axis)), col='red')
lines(x_axis, predict(fit3, data.frame(x=x_axis)), col='purple')
lines(x_axis, predict(fit4, data.frame(x=x_axis)), col='blue')
lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='orange')
lines(x_axis, predict(fit6, data.frame(x=x_axis)), col='black')
lines(x_axis, predict(fit7, data.frame(x=x_axis)), col='grey')
lines(x_axis, predict(fit8, data.frame(x=x_axis)), col='orange')
lines(x_axis, predict(fit9, data.frame(x=x_axis)), col='black')
lines(x_axis, predict(fit10, data.frame(x=x_axis)), col='grey')
lines(x_axis, predict(fit11, data.frame(x=x_axis)), col='green')
lines(x_axis, predict(fit12, data.frame(x=x_axis)), col='red')
lines(x_axis, predict(fit13, data.frame(x=x_axis)), col='purple')
lines(x_axis, predict(fit25, data.frame(x=x_axis)), col='black')
lines(x_axis, predict(fit35, data.frame(x=x_axis)), col='grey')
lines(x_axis, predict(fit100, data.frame(x=x_axis)), col='grey')

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
plot(df$x, df$y, pch=19, xlab='x', ylab='y')

#define x-axis values
x_axis <- seq(1, 500, length=500)

#add curve of fourth-degree polynomial model
lines(x_axis, predict(fit25, data.frame(x=x_axis)), col='blue')


summary(fit25)




