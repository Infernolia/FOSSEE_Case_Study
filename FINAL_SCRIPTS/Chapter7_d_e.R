rm(list=ls())

# Need the following packages for the ARMA-GARCH function.
#install.packages("tseries")
library("tseries")
#install.packages("TTR")
library("TTR")
#install.packages("dynlm")
library(dynlm)
#install.packages("FinTS")
library(FinTS)
#install.packages("fGarch")
library(fGarch)
#install.packages("rugarch")
library(rugarch)

library(stringr)
library(ggplot2)
library(DataExplorer)
library(tseries)
library("TTR")


df <- read.csv("data_final/combined_final_data.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))


df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}

# -------------------------------------------------------------------------------------------------------------------
# Smoothening time series using Kernel Regression Smoothening Total Cases
# -------------------------------------------------------------------------------------------------------------------
plot(df$x, df$total_cases, type = 'l',      col = 'blue', xlab = "Time", ylab = "Total Cases",     main = 'Kernel Smoothing')
cases <- with(df, ksmooth(x,total_cases, "normal", bandwidth = 10))
lines(df$x, cases$y, pch = 1, col = "red")  
legend("topleft", c("Actual Values","Smoothed Curve"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")


t <- (cases$y - df$total_cases)
plot(df$x, t, pch=19, xlab='Time', ylab='Difference between Actual Values and Smoothed Values',type="l",main="Total Cases")

t1 <- (cases$y - df$total_cases)^2
plot(df$x, t1, pch=19, xlab='Time', ylab='Squared Difference between Actual Values and Smoothed Values',type="l",main="Total Cases")




# -------------------------------------------------------------------------------------------------------------------
# Arch and Garch for total cases
# -------------------------------------------------------------------------------------------------------------------

ta <- t

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


plot(df$x, ta, type = 'l',      col = 'blue', xlab = "", ylab = "",     main = 'ARMA-GARCH model for Total Cases')
lines(df$x, FV, pch = 1, col = "red")  
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
plot(df$x, df$total_deaths, type = 'l',      col = 'blue', xlab = "Time", ylab = "Total Deaths",     main = 'Kernel Smoothing')
deaths <- with(df, ksmooth(x,total_deaths, "normal", bandwidth = 10))
lines(df$x, deaths$y, pch = 1, col = "red")  
legend("topleft", c("Actual Values","Smoothed Curve"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")

t <- (deaths$y - df$total_deaths)
plot(df$x, t, pch=19, xlab='Time', ylab='Difference between Actual Values and Smoothed Values',type="l",main="Total Deaths")

t1 <- (deaths$y - df$total_deaths)^2
plot(df$x, t1, pch=19, xlab='Time', ylab='Squared Difference between Actual Values and Smoothed Values',type="l",main="Total Deaths")

# -------------------------------------------------------------------------------------------------------------------
# Arch and Garch for total cases
# -------------------------------------------------------------------------------------------------------------------

ta <- t

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



plot(df$x, ta, type = 'l',      col = 'blue', xlab = "", ylab = "",     main = 'ARMA-GARCH model for Total Deaths')
lines(df$x, FV, pch = 1, col = "red") 
legend("topleft", c("Noise","Fitted Values"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")

#=========================================================================================================
rm(list=ls())
df <- read.csv("data_final/combined_final_data.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))


df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}
#=========================================================================================================

# -------------------------------------------------------------------------------------------------------------------
# Smoothening time series using Kernel Regression Smoothening Total Strains
# -------------------------------------------------------------------------------------------------------------------
plot(df$x, df$total_strains, type = 'l',      col = 'blue', xlab = "Time", ylab = "Total Strains",     main = 'Kernel Smoothing')
strains <- with(df, ksmooth(x,total_strains, "normal", bandwidth = 10))
lines(df$x, strains$y, pch = 1, col = "red")  
legend("topleft", c("Actual Values","Smoothed Curve"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")

t <- (strains$y - df$total_strains)
plot(df$x, t, pch=19, xlab='Time', ylab='Difference between Actual Values and Smoothed Values',type="l",main="Total Strains")

t1 <- (strains$y - df$total_strains)^2
plot(df$x, t1, pch=19, xlab='Time', ylab='Squared Difference between Actual Values and Smoothed Values',type="l",main="Total Strains")

# -------------------------------------------------------------------------------------------------------------------
# Arch and Garch for total cases
# -------------------------------------------------------------------------------------------------------------------

ta <- t

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



plot(df$x, ta, type = 'l',      col = 'blue', xlab = "", ylab = "",     main = 'ARMA-GARCH model for Total Strains')
lines(df$x, FV, pch = 1, col = "red") 
legend("topleft", c("Noise","Fitted Values"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")

#=========================================================================================================
rm(list=ls())
df <- read.csv("data_final/combined_final_data.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))


df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}
#=========================================================================================================

# -------------------------------------------------------------------------------------------------------------------
# Smoothening time series using Kernel Regression Smoothening Total Severity
# -------------------------------------------------------------------------------------------------------------------
plot(df$x, df$Total_severity, type = 'l',     col = 'blue', xlab = "Time", ylab = "Total Severity",     main = 'Kernel Smoothing')
severity <- with(df, ksmooth(x,Total_severity, "normal", bandwidth = 10))
lines(df$x, severity$y, pch = 1, col = "red")  
legend("topleft", c("Actual Values","Smoothed Curve"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")


t <- (severity$y - df$Total_severity)
plot(df$x, t, pch=19, xlab='Time', ylab='Difference between Actual Values and Smoothed Values',type="l",main="Total Severity")

t1 <- (severity$y - df$Total_severity)^2
plot(df$x, t1, pch=19, xlab='Time', ylab='Squared Difference between Actual Values and Smoothed Values',type="l",main="Total Severity")

# -------------------------------------------------------------------------------------------------------------------
# Arch and Garch for total cases
# -------------------------------------------------------------------------------------------------------------------

ta <- t

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



plot(df$x, ta, type = 'l',        col = 'blue', xlab = "", ylab = "",     main = 'ARMA-GARCH model for Total Severity')
lines(df$x, FV, pch = 1, col = "red") 
legend("topleft", c("Noise","Fitted Values"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")

#=========================================================================================================
rm(list=ls())
df <- read.csv("data_final/combined_final_data.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))


df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}
#=========================================================================================================


# -------------------------------------------------------------------------------------------------------------------
# Smoothening time series using Kernel Regression Smoothening Alpha
# -------------------------------------------------------------------------------------------------------------------
plot(df$x, df$alpha_strains, type = 'l',      col = 'blue', xlab = "Time", ylab = "Alpha Strains",     main = 'Kernel Smoothing')
alpha <- with(df, ksmooth(x,alpha_strains, "normal", bandwidth = 5))
lines(df$x, alpha$y, pch = 1, col = "red")  
legend("topleft", c("Actual Values","Smoothed Curve"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")

t <- (alpha$y - df$alpha_strains)
plot(df$x, t, pch=19, xlab='Time', ylab='Difference between Actual Values and Smoothed Values',type="l",main="Alpha Strains")

t1 <- (alpha$y - df$alpha_strains)^2
plot(df$x, t1, pch=19, xlab='Time', ylab='Squared Difference between Actual Values and Smoothed Values',type="l",main="Alpha Strains")


# -------------------------------------------------------------------------------------------------------------------
# Arch and Garch for total cases
# -------------------------------------------------------------------------------------------------------------------

ta <- t

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



plot(df$x, ta, type = 'l',      col = 'blue', xlab = "", ylab = "", main = 'ARMA-GARCH model for Alpha Strains')
lines(df$x, FV, pch = 1, col = "red") 
legend("topleft", c("Noise","Fitted Values"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")

#=========================================================================================================
rm(list=ls())
df <- read.csv("data_final/combined_final_data.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))


df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}
#=========================================================================================================

# -------------------------------------------------------------------------------------------------------------------
# Smoothening time series using Kernel Regression Smoothening Beta
# -------------------------------------------------------------------------------------------------------------------
plot(df$x, df$beta_strains, type = 'l',      col = 'blue', xlab = "Time", ylab = "Beta Strains",     main = 'Kernel Smoothing')
beta <- with(df, ksmooth(x,beta_strains, "normal", bandwidth = 10))
lines(df$x, beta$y, pch = 1, col = "red")  
legend("topleft", c("Actual Values","Smoothed Curve"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")

t <- (beta$y - df$beta_strains)
plot(df$x, t, pch=19, xlab='Time', ylab='Difference between Actual Values and Smoothed Values',type="l",main="Beta Strains")

t1 <- (beta$y - df$beta_strains)^2
plot(df$x, t1, pch=19, xlab='Time', ylab='Squared Difference between Actual Values and Smoothed Values',type="l",main="Beta Strains")


# -------------------------------------------------------------------------------------------------------------------
# Arch and Garch for total cases
# -------------------------------------------------------------------------------------------------------------------

ta <- t

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



plot(df$x, ta, type = 'l',     col = 'blue', xlab = "", ylab = "",     main = 'ARMA-GARCH model for Beta Strains')
lines(df$x, FV, pch = 1, col = "red")
legend("topleft", c("Noise","Fitted Values"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")

#=========================================================================================================
rm(list=ls())
df <- read.csv("data_final/combined_final_data.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))


df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}
#=========================================================================================================

# -------------------------------------------------------------------------------------------------------------------
# Smoothening time series using Kernel Regression Smoothening Gamma
# -------------------------------------------------------------------------------------------------------------------
plot(df$x, df$gamma_strains, type = 'l',      col = 'blue', xlab = "Time", ylab = "Gamma Strains",     main = 'Kernel Smoothing')
gamma <- with(df, ksmooth(x,gamma_strains, "normal", bandwidth = 10))
lines(df$x, gamma$y, pch = 1, col = "red")
legend("topleft", c("Actual Values","Smoothed Curve"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")

t <- (gamma$y - df$gamma_strains)
plot(df$x, t, pch=19, xlab='Time', ylab='Difference between Actual Values and Smoothed Values',type="l",main="Gamma Strains")


t1 <- (gamma$y - df$gamma_strains)^2
plot(df$x, t1, pch=19, xlab='Time', ylab='Squared Difference between Actual Values and Smoothed Values',type="l",main="Gamma Strainss")

# -------------------------------------------------------------------------------------------------------------------
# Arch and Garch for total cases
# -------------------------------------------------------------------------------------------------------------------

ta <- t

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



plot(df$x, ta, type = 'l',      col = 'blue', xlab = "", ylab = "",  main = 'ARMA-GARCH model for Gamma Strains')
lines(df$x, FV, pch = 1, col = "red") 
legend("topleft", c("Noise","Fitted Values"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")

#=========================================================================================================
rm(list=ls())
df <- read.csv("data_final/combined_final_data.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))


df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}
#=========================================================================================================

# -------------------------------------------------------------------------------------------------------------------
# Smoothening time series using Kernel Regression Smoothening Delta
# -------------------------------------------------------------------------------------------------------------------
plot(df$x, df$delta_strains, type = 'l',     col = 'blue', xlab = "Time", ylab = "Delta Strains",     main = 'Kernel Smoothing')
delta <- with(df, ksmooth(x,delta_strains, "normal", bandwidth = 10))
lines(df$x, delta$y, pch = 1, col = "red")  
legend("topleft", c("Actual Values","Smoothed Curve"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")

t <- (delta$y - df$delta_strains)
plot(df$x, t, pch=19, xlab='Time', ylab='Difference between Actual Values and Smoothed Values',type="l",main="Delta Strains")

t1 <- (delta$y - df$delta_strains)^2
plot(df$x, t1, pch=19, xlab='Time', ylab='Squared Difference between Actual Values and Smoothed Values',type="l",main="Delta Strains")


# -------------------------------------------------------------------------------------------------------------------
# Arch and Garch for total cases
# -------------------------------------------------------------------------------------------------------------------

ta <- t

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



plot(df$x, ta, type = 'l',      col = 'blue', xlab = "", ylab = "",   main = 'ARMA-GARCH model for Delta Strains')
lines(df$x, FV, pch = 1, col = "red") 
legend("topleft", c("Noise","Fitted Values"),lty = c(1,1), lwd = 2, col = c('blue','red'), bty = "n")
