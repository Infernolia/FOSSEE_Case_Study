rm(list=ls())
library(stringr)
library(ggplot2)
library(DataExplorer)
library(tseries)
library("TTR")



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


rm(list=ls())

#-----------------------------------------------------------------------------------
# Stationarity tests
#-----------------------------------------------------------------------------------

df <- read.csv("data_final/combined_final_data.csv") # Reading the number of cases
df <- subset(df,select = -c(1))

t1 <- diff(df$total_cases)
t2 <- diff(df$total_deaths)
t3 <- diff(df$total_strains)

adf1 = adf.test(t1)
adf1

adf2 = adf.test(t2)
adf2

adf3 = adf.test(t3)
adf3


t4 <- diff(t1)
adf4 = adf.test(t4)
adf4

t4 <- c(t4,0)
t4 <- c(t4,0)
t2 <- c(t2,0)
t3 <- c(t3,0)

ggplot() + 
  geom_line(aes(x=df$Date,y=t4,group = 1),color='blue') + 
  geom_line(aes(x=df$Date,y=t2,group = 1),color='red') +
  geom_line(aes(x=df$Date,y=t3,group = 1),color='green') +
  ylab('Values')+xlab('date')

ggplot() + 
  geom_line(aes(x=df$Date,y=t4,group = 1),color='blue') + 
  ylab('Total_Cases')+xlab('date')


ggplot() + 
  geom_line(aes(x=df$Date,y=t2,group = 1),color='red') +
  ylab('Total_Deaths')+xlab('date')

ggplot() + 
  geom_line(aes(x=df$Date,y=t3,group = 1),color='green') +
  ylab('Total_Strains')+xlab('date')


#-----------------------------------------------------------------------------------
# Plotting on same axis
#-----------------------------------------------------------------------------------
df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}

par(mar = c(5, 4, 4, 4) + 0.3)              # Additional space for second y-axis
plot(df$x, df$total_cases, pch = 16, col = 2)              # Create first plot
par(new = TRUE)                             # Add new plot
plot(df$x, df$total_deaths, pch = 17, col = 3,              # Create second plot without axes
     axes = FALSE, xlab = "", ylab = "")
axis(side = 4, at = pretty(range(df$total_deaths)))      # Add second axis
mtext("total_deaths", side = 4, line = 3)             # Add second axis label


#-----------------------------------------------------------------------------------
# Smoothening curves with Kernel Regression with validation and comparing with other regressions
#-----------------------------------------------------------------------------------


ggplot() + 
  geom_line(aes(x=df$Date,y=df$total_cases,group = 1),color='blue') + 
  geom_line(aes(x=df$Date,y=df$total_deaths,group = 1),color='red') +
  geom_line(aes(x=df$Date,y=df$total_strains,group = 1),color='green') +
  geom_line(aes(x=df$Date,y=df$Total_severity,group = 1),color='purple') +
  ylab('Values')+xlab('date')




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

legend("bottomright", c("supsmu", "loess", "ksmooth"),
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

legend("bottomright", c("supsmu", "loess", "ksmooth"),
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

legend("bottomright", c("supsmu", "loess", "ksmooth"),
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

legend("bottomright", c("supsmu", "loess", "ksmooth"),
       lty = 1:3, lwd = 2, col = c("black", "red", "blue"), bty = "n")



#-----------------------------------------------------------------------------------


plot(df$x, df$total_cases,xlab = "Time", ylab = "Total Severity")
lines(cases, lwd = 2, lty = 3, col = "blue")#cases
lines(deaths, lwd = 2, lty = 3, col = "green")#deaths
lines(strains, lwd = 2, lty = 3, col = "red")#strains
lines(severity, lwd = 2, lty = 3, col = "purple")#severity

legend("bottomright", c("cases", "deaths", "strains","severity"),
       lty = 1:4, lwd = 2, col = c("blue","green","purple"), bty = "n")

par(mar = c(5, 4, 4, 4) + 0.3)              # Additional space for second y-axis
plot(df$x, df$total_cases, pch = 1, col = 2)              # Create first plot
lines(cases, lwd = 2, lty = 3, col = "blue")#cases
lines(deaths, lwd = 2, lty = 3, col = "black")#deaths
par(new = TRUE)                             # Add new plot
plot(df$x, df$total_strains, pch = 1, col = 3,              # Create second plot without axes
     axes = FALSE, xlab = "", ylab = "")
lines(strains, lwd = 2, lty = 3, col = "red")#strains
lines(severity, lwd = 2, lty = 3, col = "purple")#severity
axis(side = 4, at = pretty(range(df$total_deaths)))      # Add second axis
# Add second axis label


#-----------------------------------------------------------------------------------
# Comparing the trends of 4 variants
#-----------------------------------------------------------------------------------

ggplot() + 
  geom_line(aes(x=df$Date,y=df$alpha_strains,group = 1),color='blue') + 
  geom_line(aes(x=df$Date,y=df$beta_strains,group = 1),color='red') +
  geom_line(aes(x=df$Date,y=df$gamma_strains,group = 1),color='green') +
  geom_line(aes(x=df$Date,y=df$delta_strains,group = 1),color='purple') +
  ylab('Values')+xlab('date')
+
legend("bottomright", c( "beta", "alpha","gamma","delta"),
       lty = 1, lwd = 2, col = c("red","blue","green","purple"), bty = "n")

#-----------------------------------------------------------------------------------
# Smoothening the trends of 4 variants
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

legend("bottomright", c("supsmu", "loess", "ksmooth"),
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

legend("bottomright", c("supsmu", "loess", "ksmooth"),
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

legend("bottomright", c("supsmu", "loess", "ksmooth"),
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

legend("bottomright", c("supsmu", "loess", "ksmooth"),
       lty = 1:3, lwd = 2, col = c("black", "red", "blue"), bty = "n")



#-----------------------------------------------------------------------------------


plot(df$x, df$alpha_strains,xlab = "Time", ylab = "Total Severity")
lines(alpha, lwd = 2, lty = 3, col = "blue")#cases
lines(beta, lwd = 2, lty = 3, col = "green")#deaths
lines(gamma, lwd = 2, lty = 3, col = "red")#strains
lines(delta, lwd = 2, lty = 3, col = "purple")#severity

legend("bottomright", c("alpha", "beta", "gamma","delta"),
       lty = 1, lwd = 2, col = c("red","blue","green","purple"), bty = "n")

par(mar = c(5, 4, 4, 4) + 0.3)              # Additional space for second y-axis
plot(df$x, df$alpha_strains, pch = 1, col = 2)              # Create first plot
lines(alpha, lwd = 2, lty = 3, col = "blue")#cases
lines(beta, lwd = 2, lty = 3, col = "black")#deaths
par(new = TRUE)                             # Add new plot
plot(df$x, df$gamma_strains, pch = 1, col = 3,              # Create second plot without axes
     axes = FALSE, xlab = "", ylab = "")
lines(gamma, lwd = 2, lty = 3, col = "red")#strains
lines(delta, lwd = 2, lty = 3, col = "purple")#severity
axis(side = 4, at = pretty(range(df$beta_strains)))      # Add second axis
# Add second axis label



# -------------------------------------------------------------------------------------------------------------------
# Calculating ACF of smoothened strains trends
# -------------------------------------------------------------------------------------------------------------------




plot.acf <- function(ACFobj) {
  rr <- ACFobj$acf[-1]
  kk <- length(rr)
  nn <- ACFobj$n.used
  plot(seq(kk), rr, type = "h", lwd = 2, yaxs = "i", xaxs = "i", 
       ylim = c(floor(min(rr)), 1), xlim = c(0, kk + 1), xlab = "Lag", 
       ylab = "Correlation", las = 1)
  abline(h = -1/nn + c(-2, 2)/sqrt(nn), lty = "dashed", col = "blue")
  abline(h = 0)
}


# Cases and delta strains

df_temp <- rbind(cases$y,delta$y)
View(df_temp)


df_acf <- acf(df_temp, lag.max = 36)

plot.acf(df_acf,main="Auto-correlation between cases and delta strains")


df_acf1 <- acf(df_temp, lag.max = 36, type="covariance")

plot.acf(df_acf1,main="Auto-covariance between cases and delta strains")



# -------------------------------------------------------------------------------------------------------------------

# Deaths and delta strains

df_temp <- rbind(deaths$y,delta$y)
View(df_temp)


df_acf <- acf(df_temp, lag.max = 36)

plot.acf(df_acf,main="Auto-correlation between deaths and delta strains")


df_acf1 <- acf(df_temp, lag.max = 36, type="covariance")

plot.acf(df_acf1,main="Auto-covariance between deaths and delta strains")

# -------------------------------------------------------------------------------------------------------------------
# Finding residuals
# -------------------------------------------------------------------------------------------------------------------

#Between alpha and beta

ccf(alpha$y, log(alpha$y), ylab = "Cross-correlation")



model <- lm(deaths$y ~ df$x)

#view model summary
summary(model) 


model <- lm(cases$y ~ df$x)

#view model summary
summary(model) 

model <- lm(delta$y ~ df$x)

#view model summary
summary(model) 

