rm(list=ls())
library(stringr)
library(ggplot2)
library(DataExplorer)
library(tseries)
library("TTR")
#install.packages("dynlm")
library(dynlm)
#install.packages("FinTS")
library(FinTS)
#install.packages("fGarch")
library(fGarch)
#install.packages("rugarch")
library(rugarch)

df <- read.csv("data_final/combined_final_data.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))


df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}

# -------------------------------------------------------------------------------------------------------------------
# Smoothening time series using Kernel Regression Smoothening Total Cases
# -------------------------------------------------------------------------------------------------------------------
plot(c(150:300), df$total_cases[150:300], type = 'l',col = 'blue', xlab = "Time", ylab = "Total Cases Peak 1",     main = 'Kernel Smoothing')
cases <- with(df, ksmooth(x,total_cases, "normal", bandwidth = 10))
lines(c(150:300), cases$y[150:300], pch = 1, col = "red")  
legend("topleft", c("Actual Values","Smoothed Curve"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")

t <- (cases$y - df$total_cases)
plot(c(150:300), t[150:300], pch=19, xlab='Time', ylab='Difference between Actual Values and Smoothed Values',type="l",main="Total Cases")

t1 <- (cases$y - df$total_cases)^2
plot(c(150:300), t1[150:300], pch=19, xlab='Time', ylab='Squared Difference between Actual Values and Smoothed Values',type="l",main="Total Cases")

rm(setdiff(ls(),cases$y))

# -------------------------------------------------------------------------------------------------------------------
# Arch and Garch for total cases
# -------------------------------------------------------------------------------------------------------------------

ta <- t[150:300]

#  ARCH Modelling for the square of errors

#  To stop the summarized output generated from function execution
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

country <- na.omit(ta)
model <- quiet(garch(x = country,order = c(0,2),trace = F))
summ <- summary(model)
t <- as.data.frame(summ$coef[2,])
col1 <- c(rownames(t), "Ljung-Box X-squared", "Ljung-Box p.value")
col2 <- c(t[,1], as.numeric(summ$l.b.test$statistic), as.numeric(summ$l.b.test$p.value))
dftemp <- data.frame("Coeff"=col1, "Value"=col2)
colnames(dftemp) <- c("Value")

View(dftemp)





Model <- function(Country,Order) 
{
  #  Inputs
  m <- Order[1]
  n <- Order[2]
  p <- Order[3]
  q <- Order[4]
  m <- as.numeric(m)
  n <- as.numeric(n)
  p <- as.numeric(p)
  q <- as.numeric(q)
  
  #  Model
  Garch <- suppressWarnings(ugarchspec(variance.model = list(garchOrder = c(m,n)), mean.model = list(armaOrder=c(p,q))))
  Fit <- suppressWarnings(ugarchfit(Garch, Country))
  
  #  Results
  
  FV <- Fit@fit$fitted.values
  
  
  if(!is.null(FV)) {
    pred.error.sq <- (Country - FV)^2
    sum.pred.error.sq <- sum(pred.error.sq)
    return(sum.pred.error.sq)
  }
  
  else {
    return(NULL)
  }
  
}

#  All possible input combinations
List <- with(expand.grid(0:3,0:3,0:3,0:3), paste(Var1, Var2, Var3, Var4))
Combinations <- matrix(0L,nrow = length(List),ncol = 4)
for(i in 1:length(List))
{
  Combinations[i,] <- as.integer(unlist(strsplit(List[i]," ")))
}


Country <- NULL
Output <- NULL

for(j in 1:nrow(Combinations))
{
  Temp <- tryCatch(Model(ta,Combinations[j,]),error = function(err){return(NULL)})
  print(Temp)
  cat('Combination',j,': ',Combinations[j,],'\n')
  if(!is.null(Temp)){Country[j] <- Temp}else{Country[j] <- NA}
}
print(Output)
Output <- rbind(Output,Country)


View(Output)

best.fit <- NULL
smallest.error <- min(Output[1,], na.rm = TRUE)
index <- which.min(Output[1,])
best.fit <- rbind(best.fit,Combinations[index,])
View(best.fit)

smallest.pred.error <- c()

m <- best.fit[1]
n <- best.fit[2]
p <- best.fit[3]
q <- best.fit[4]


Garch <- suppressWarnings(ugarchspec(variance.model = list(garchOrder = c(m,n)), mean.model = list(armaOrder=c(p,q))))
Fit <- suppressWarnings(ugarchfit(Garch, ta))

print(i)
FV <- Fit@fit$fitted.values # stores fitted values for ith country

if(is.null(FV)) 
  smallest.pred.error[i] <- NA;
else
{  smallest.pred.error[i] <- (ta - FV)^2 }# Calculates the mean square error



plot(df$x[150:300], ta, type = 'l',      col = 'blue', xlab = "", ylab = "",      main = 'ARMA-GARCH model for Total Cases Peak 1')
lines(df$x[150:300], FV, pch = 1, col = "red")  
legend("topleft", c("Noise","Fitted Values"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")


#=========================================================================================================
rm(list=ls())
df <- read.csv("data_final/combined_final_data.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))


df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}
#=========================================================================================================
# -------------------------------------------------------------------------------------------------------------------
# Smoothening time series using Kernel Regression Smoothening Total Cases Peak 2
# -------------------------------------------------------------------------------------------------------------------
plot(df$x[400:430], df$total_cases[400:430], type = 'l',      col = 'blue', xlab = "Time", ylab = "Total Cases Peak 2",     main = 'Kernel Smoothing')
cases <- with(df, ksmooth(x,total_cases, "normal", bandwidth = 10))
lines(c(400:430), cases$y[400:430], pch = 1, col = "red")  
legend("topleft", c("Actual Values","Smoothed Curve"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")

t <- (cases$y - df$total_cases)
plot(c(400:430), t[400:430], pch=19, xlab='Time', ylab='Difference between Actual Values and Smoothed Values',type="l",main="Total Cases Peak 2")

t1 <- (cases$y - df$total_cases)^2
plot(c(400:430), t1[400:430], pch=19, xlab='Time', ylab='Squared Difference between Actual Values and Smoothed Values',type="l",main="Total Cases Peak 2")

rm(setdiff(ls(),cases$y))

# -------------------------------------------------------------------------------------------------------------------
# Arch and Garch for total cases
# -------------------------------------------------------------------------------------------------------------------

ta <- t[400:430]

#  ARCH Modelling for the square of errors

#  To stop the summarized output generated from function execution
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

country <- na.omit(ta)
model <- quiet(garch(x = country,order = c(0,2),trace = F))
summ <- summary(model)
t <- as.data.frame(summ$coef[2,])
col1 <- c(rownames(t), "Ljung-Box X-squared", "Ljung-Box p.value")
col2 <- c(t[,1], as.numeric(summ$l.b.test$statistic), as.numeric(summ$l.b.test$p.value))
dftemp <- data.frame("Coeff"=col1, "Value"=col2)
colnames(dftemp) <- c("Value")

View(dftemp)





Model <- function(Country,Order) 
{
  #  Inputs
  m <- Order[1]
  n <- Order[2]
  p <- Order[3]
  q <- Order[4]
  m <- as.numeric(m)
  n <- as.numeric(n)
  p <- as.numeric(p)
  q <- as.numeric(q)
  
  #  Model
  Garch <- suppressWarnings(ugarchspec(variance.model = list(garchOrder = c(m,n)), mean.model = list(armaOrder=c(p,q))))
  Fit <- suppressWarnings(ugarchfit(Garch, Country))
  
  #  Results
  
  FV <- Fit@fit$fitted.values
  
  
  if(!is.null(FV)) {
    pred.error.sq <- (Country - FV)^2
    sum.pred.error.sq <- sum(pred.error.sq)
    return(sum.pred.error.sq)
  }
  
  else {
    return(NULL)
  }
  
}

#  All possible input combinations
List <- with(expand.grid(0:3,0:3,0:3,0:3), paste(Var1, Var2, Var3, Var4))
Combinations <- matrix(0L,nrow = length(List),ncol = 4)
for(i in 1:length(List))
{
  Combinations[i,] <- as.integer(unlist(strsplit(List[i]," ")))
}


Country <- NULL
Output <- NULL

for(j in 1:nrow(Combinations))
{
  Temp <- tryCatch(Model(ta,Combinations[j,]),error = function(err){return(NULL)})
  print(Temp)
  cat('Combination',j,': ',Combinations[j,],'\n')
  if(!is.null(Temp)){Country[j] <- Temp}else{Country[j] <- NA}
}
print(Output)
Output <- rbind(Output,Country)


View(Output)

best.fit <- NULL
smallest.error <- min(Output[1,], na.rm = TRUE)
index <- which.min(Output[1,])
best.fit <- rbind(best.fit,Combinations[index,])
View(best.fit)

smallest.pred.error <- c()

m <- best.fit[1]
n <- best.fit[2]
p <- best.fit[3]
q <- best.fit[4]


Garch <- suppressWarnings(ugarchspec(variance.model = list(garchOrder = c(m,n)), mean.model = list(armaOrder=c(p,q))))
Fit <- suppressWarnings(ugarchfit(Garch, ta))

print(i)
FV <- Fit@fit$fitted.values # stores fitted values for ith country

if(is.null(FV)) 
  smallest.pred.error[i] <- NA;
else
{  smallest.pred.error[i] <- (ta - FV)^2 }# Calculates the mean square error



plot(df$x[400:430], ta, type = 'l',      col = 'blue', xlab = "", ylab = "",    main = 'ARMA-GARCH model for Total Cases Peak 2')
lines(df$x[400:430], FV, pch = 1, col = "red")  
legend("topleft", c("Noise","Fitted Values"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")



#=========================================================================================================
rm(list=ls())
df <- read.csv("data_final/combined_final_data.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))


df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}
#=========================================================================================================


# -------------------------------------------------------------------------------------------------------------------
# Smoothening time series using Kernel Regression Smoothening Total Deaths
# -------------------------------------------------------------------------------------------------------------------
plot(df$x[-c(90:110)], df$total_deaths[-c(90:110)], type = 'l',    col = 'blue', xlab = "Time", ylab = "Total Deaths Piece with Peak Removed",     main = 'Kernel Smoothing')
deaths <- with(df, ksmooth(x,total_deaths, "normal", bandwidth = 10))
lines(df$x[-c(90:110)], deaths$y[-c(90:110)], pch = 1, col = "red")  
legend("topleft", c("Actual Values","Smoothed Curve"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")

t <- (deaths$y - df$total_deaths)
plot(df$x[-c(90:110)], t[-c(90:110)], pch=19, xlab='Time', ylab='Difference between Actual Values and Smoothed Values',type="l",main="Total Deaths Piece with Peak Removed")

t1 <- (deaths$y - df$total_deaths)^2
plot(df$x[-c(90:110)], t1[-c(90:110)], pch=19, xlab='Time', ylab='Squared Difference between Actual Values and Smoothed Values',type="l",main="Total Deaths Piece with Peak Removed")

# -------------------------------------------------------------------------------------------------------------------
# Arch and Garch for total cases
# -------------------------------------------------------------------------------------------------------------------

ta <- t[-c(90:110)]

#  ARCH Modelling for the square of errors

#  To stop the summarized output generated from function execution
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

country <- na.omit(ta)
model <- quiet(garch(x = country,order = c(0,2),trace = F))
summ <- summary(model)
t <- as.data.frame(summ$coef[2,])
col1 <- c(rownames(t), "Ljung-Box X-squared", "Ljung-Box p.value")
col2 <- c(t[,1], as.numeric(summ$l.b.test$statistic), as.numeric(summ$l.b.test$p.value))
dftemp <- data.frame("Coeff"=col1, "Value"=col2)
colnames(dftemp) <- c("Value")

View(dftemp)





Model <- function(Country,Order) 
{
  #  Inputs
  m <- Order[1]
  n <- Order[2]
  p <- Order[3]
  q <- Order[4]
  m <- as.numeric(m)
  n <- as.numeric(n)
  p <- as.numeric(p)
  q <- as.numeric(q)
  
  #  Model
  Garch <- suppressWarnings(ugarchspec(variance.model = list(garchOrder = c(m,n)), mean.model = list(armaOrder=c(p,q))))
  Fit <- suppressWarnings(ugarchfit(Garch, Country))
  
  #  Results
  
  FV <- Fit@fit$fitted.values
  
  
  if(!is.null(FV)) {
    pred.error.sq <- (Country - FV)^2
    sum.pred.error.sq <- sum(pred.error.sq)
    return(sum.pred.error.sq)
  }
  
  else {
    return(NULL)
  }
  
}

#  All possible input combinations
List <- with(expand.grid(0:3,0:3,0:3,0:3), paste(Var1, Var2, Var3, Var4))
Combinations <- matrix(0L,nrow = length(List),ncol = 4)
for(i in 1:length(List))
{
  Combinations[i,] <- as.integer(unlist(strsplit(List[i]," ")))
}


Country <- NULL
Output <- NULL

for(j in 1:nrow(Combinations))
{
  Temp <- tryCatch(Model(ta,Combinations[j,]),error = function(err){return(NULL)})
  print(Temp)
  cat('Combination',j,': ',Combinations[j,],'\n')
  if(!is.null(Temp)){Country[j] <- Temp}else{Country[j] <- NA}
}
print(Output)
Output <- rbind(Output,Country)


View(Output)

best.fit <- NULL
smallest.error <- min(Output[1,], na.rm = TRUE)
index <- which.min(Output[1,])
best.fit <- rbind(best.fit,Combinations[index,])
View(best.fit)

smallest.pred.error <- c()

m <- best.fit[1]
n <- best.fit[2]
p <- best.fit[3]
q <- best.fit[4]


Garch <- suppressWarnings(ugarchspec(variance.model = list(garchOrder = c(m,n)), mean.model = list(armaOrder=c(p,q))))
Fit <- suppressWarnings(ugarchfit(Garch, ta))

print(i)
FV <- Fit@fit$fitted.values # stores fitted values for ith country

if(is.null(FV)) 
  smallest.pred.error[i] <- NA;
else
{  smallest.pred.error[i] <- (ta - FV)^2 }# Calculates the mean square error



plot(df$x[-c(90:110)], ta, type = 'l',      col = 'blue', xlab = "", ylab = "",     main = 'ARMA-GARCH model for Total Deaths Piece with Peak Removed')
lines(df$x[-c(90:110)], FV, pch = 1, col = "red") 
legend("topleft", c("Noise","Fitted Values"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")
#=========================================================================================================


library(forecast)


#=========================================================================================================
rm(list=ls())
df <- read.csv("data_final/combined_final_data.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))


df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}
#=========================================================================================================
# -------------------------------------------------------------------------------------------------------------------
# Smoothening time series using Kernel Regression Smoothening Total Cases
# -------------------------------------------------------------------------------------------------------------------
plot(df$x[400:430], df$total_cases[400:430], type = 'l',      col = 'blue', xlab = "Time", ylab = "Total Cases Peak 2",     main = 'Kernel Smoothing')
cases <- with(df, ksmooth(x,total_cases, "normal", bandwidth = 10))
lines(df$x[400:430], cases$y[400:430], pch = 1, col = "red")  

t <- (cases$y - df$total_cases)
plot(df$x[400:430], t[400:430], pch=19, xlab='Time', ylab='Difference between Actual Values and Smoothed Values',type="l",main="Total Cases")

fit<-auto.arima(t[400:430])

plot(df$x[400:430], t[400:430], type = 'l', col = 'blue', xlab = "", ylab = "",  ,main = 'Auto-ARIMA model for Total Cases Peak 2')
lines(df$x[400:430],fit$fitted, pch=19 , col = "red" )
legend("topleft", c("Noise","Fitted Values"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")