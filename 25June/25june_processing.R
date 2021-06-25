rm(list=ls())
library(stringr)
library(ggplot2)
library(DataExplorer)
library(tseries)
library("TTR")

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


df <- read.csv("25june_final_data.csv") # Reading the number of cases
df <- subset(df,select = -c(1))


df1 <- read.csv("final_df2_date_cleaned.csv") # Reading the number of strains
df1 <- subset(df1,select = -c(1))
df1$Datee <- 2000-00-00
for(i in 1:length(rownames(df1)))
{
  df1$Datee[i] <-paste(paste(df1$year[i],substrRight(paste("0",df1$month[i], sep =""),2), sep ="-"),substrRight(paste("0",df1$day[i], sep =""),2), sep ="-")
}


ggplot() + 
  geom_line(aes(x=df$Date,y=df$State1_Confirmed,group = 1),color='blue') + 
  geom_line(aes(x=df$Date,y=df$State1_Deaths,group = 1),color='red') +
  ylab('Values')+xlab('date')



t <- intersect(df$Date,df1$Datee)
length(t)

strains <- df1[df1$Datee %in% t,]
strains_freq <- as.data.frame(table(strains$Datee))
colnames(strains_freq) <- c("Date","Strains")



cases <- df[df$Date %in% t,]
cases <- subset(cases,select = c(1,74,75))



final_df <- merge(strains_freq,cases,by="Date")
View(final_df)


ggplot() + 
  geom_line(aes(x=final_df$Date,y=final_df$total_cases,group = 1),color='blue') + 
  geom_line(aes(x=final_df$Date,y=final_df$total_deaths,group = 1),color='red') +
  geom_line(aes(x=final_df$Date,y=final_df$Strains,group = 1),color='green') +
  ylab('Values')+xlab('date')



t1 <- diff(final_df$total_cases)
t2 <- diff(final_df$total_deaths)
t3 <- diff(final_df$Strains)

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
  geom_line(aes(x=final_df$Date,y=t4,group = 1),color='blue') + 
  geom_line(aes(x=final_df$Date,y=t2,group = 1),color='red') +
  geom_line(aes(x=final_df$Date,y=t3,group = 1),color='green') +
  ylab('Values')+xlab('date')

ggplot() + 
  geom_line(aes(x=final_df$Date,y=t4,group = 1),color='blue') + 
  ylab('Total_Cases')+xlab('date')


ggplot() + 
  geom_line(aes(x=final_df$Date,y=t2,group = 1),color='red') +
  ylab('Total_Deaths')+xlab('date')

ggplot() + 
  geom_line(aes(x=final_df$Date,y=t3,group = 1),color='green') +
  ylab('Total_Strains')+xlab('date')





