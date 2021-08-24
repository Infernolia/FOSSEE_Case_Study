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


kern1 <- with(df, ksmooth.gcv(x,total_cases))
kern1$df

# plot data
plot(df$x, df$total_cases,xlab = "Time", ylab = "Total Cases")

lines(locavg, lwd = 2)
lines(locreg, lwd = 2, lty = 2, col = "red")
lines(kern1, lwd = 2, lty = 3, col = "blue")

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


kern2 <- with(df, ksmooth.gcv(x,total_deaths))
kern2$df

# plot data
plot(df$x, df$total_deaths,xlab = "Time", ylab = "Total Deaths")

lines(locavg, lwd = 2)
lines(locreg, lwd = 2, lty = 2, col = "red")
lines(kern2, lwd = 2, lty = 3, col = "blue")

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


kern3 <- with(df, ksmooth.gcv(x,total_strains))
kern3$df

# plot data
plot(df$x, df$total_strains,xlab = "Time", ylab = "Total Strains")

lines(locavg, lwd = 2)
lines(locreg, lwd = 2, lty = 2, col = "red")
lines(kern3, lwd = 2, lty = 3, col = "blue")

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


kern4 <- with(df, ksmooth.gcv(x,Total_severity))
kern4$df

# plot data
plot(df$x, df$Total_severity,xlab = "Time", ylab = "Total Severity")

lines(locavg, lwd = 2)
lines(locreg, lwd = 2, lty = 2, col = "red")
lines(kern4, lwd = 2, lty = 3, col = "blue")

legend("bottomright", c("supsmu", "loess", "ksmooth"),
       lty = 1:3, lwd = 2, col = c("black", "red", "blue"), bty = "n")



#-----------------------------------------------------------------------------------


plot(df$x, df$total_cases,xlab = "Time", ylab = "Total Severity")
lines(kern1, lwd = 2, lty = 3, col = "blue")#cases
lines(kern2, lwd = 2, lty = 3, col = "green")#deaths
lines(kern3, lwd = 2, lty = 3, col = "red")#strains
lines(kern4, lwd = 2, lty = 3, col = "purple")#severity

legend("bottomright", c("cases", "deaths", "strains","severity"),
       lty = 1:4, lwd = 2, col = c("blue","green","purple"), bty = "n")

par(mar = c(5, 4, 4, 4) + 0.3)              # Additional space for second y-axis
plot(df$x, df$total_cases, pch = 1, col = 2)              # Create first plot
lines(kern1, lwd = 2, lty = 3, col = "blue")#cases
lines(kern2, lwd = 2, lty = 3, col = "black")#deaths
par(new = TRUE)                             # Add new plot
plot(df$x, df$total_strains, pch = 1, col = 3,              # Create second plot without axes
     axes = FALSE, xlab = "", ylab = "")
lines(kern3, lwd = 2, lty = 3, col = "red")#strains
lines(kern4, lwd = 2, lty = 3, col = "purple")#severity
axis(side = 4, at = pretty(range(df$total_deaths)))      # Add second axis
           # Add second axis label

