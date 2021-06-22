rm(list=ls())
#install.packages("TTR")
#install.packages("tseries")
library(tseries)
library("TTR")
library(stringr)
library(ggplot2)

df <- read.csv("final_df2_date_cleaned.csv")
df <- subset(df,select = -c(1))


data <- read.csv("india.csv")
data <- subset(data,select = c(1,2,6))


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

data$datee <- 0

for(i in 1:length(rownames(data)))
{
  
  s<- data$date[i]
  day <- substr(s,1,2)
  month <- substr(s,4,5)
  year <- substr(s,7,10)
  data$datee[i] <-paste(paste(year,substrRight(paste("0",month, sep =""),2), sep =""),substrRight(paste("0",day, sep =""),2), sep ="")
}

data <- subset(data,select = -c(1,2))


t <- intersect(df$date,data$datee)
t

df_new <- df[df$date %in% t,]
df_new_freq <- as.data.frame(table(df_new$date))
colnames(df_new_freq) <- c("Date","Strains")


data_new <- data[data$datee %in% t,]


data_new$strains <- 0


for(i in 1:length(rownames(data_new)))
{
  day <- data_new$datee[i]
  temp_df <- df_new_freq[df_new_freq$Date == day, ]
  temp_df1 <-df[df$date == day, ]
  data_new$strains[i] <- min(temp_df$Strains)

}  

View(data_new)


adf = adf.test(data_new$total_cases)
adf

adf1 = adf.test(data_new$strains)
adf1

t1 <- diff(data_new$total_cases)
t2 <- diff(data_new$strains)

adf3 = adf.test(t1)
adf3

adf4 = adf.test(t2)
adf4

t11 <- diff(t1)
t21 <- diff(t2)

adf5 = adf.test(t11)
adf5

ggplot() + 
  geom_line(aes(x=c(1:443),y=t21,group = 1),color='blue') + 
  geom_line(aes(x=c(1:443),y=t11,group = 1),color='red') +
  ylab('Values')+xlab('date')


y <- 500 + 0.4 * (t11-10)
noise <- rnorm(443, mean=10, sd=80)
noisy.y <- y + noise

plot(t11,noisy.y,col='deepskyblue4',xlab='q',main='Observed data')
lines(t11,y,col='firebrick1',lwd=1)


y1 <- 0.4 * t21
noise <- rnorm(443, mean=10, sd=80)
noisy.y <- y + noise

plot(t21,noisy.y,col='deepskyblue4',xlab='q',main='Observed data')
lines(t21,y,col='firebrick1',lwd=1)
