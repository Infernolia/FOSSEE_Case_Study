rm(list=ls())
library(stringr)
library(ggplot2)
library(DataExplorer)
library(tseries)
library("TTR")



df <- read.csv("casestudy_final_cases_data.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))

df1 <- read.csv("casestudy_final_strains_data.csv") # Reading the number of strains
df1 <- subset(df1,select = -c(1))

df2 <- as.data.frame(table(df1$date1))
colnames(df2) <- c("Date","Strains")


#Test for stationarity
for(i in 1:41)
{
  
  if(i!=6 && i!=9 && i!=11 && i!=35 && i!=38)
  {
    s <- paste0(i,"_severity.png")
    p <- paste(paste("State",i,sep=""),"_Severity",sep="")
    print(paste("State Name",i))
    adf1 = adf.test(df[,p])
    print(adf1)
    
  }
  
}

df$total_strains <- 0
for(i in 1:nrow(df))
{
  for(j in 1:nrow(df2))
  {
    if(df$Date[i]==df2$Date[j])
    {
      df$total_strains[i] <- df2$Strains[j]
    }
  }
}

View(df)

df$Total_severity <- df$Total_severity*100
df$total_cases <- df$total_cases/100
basic <-ggplot() + 
  geom_line(aes(x=df$Date,y=df$total_strains,group = 1),color='blue') + 
  geom_line(aes(x=df$Date,y=df$Total_severity,group = 1),color='red') +
  geom_line(aes(x=df$Date,y=df$total_deaths,group = 1),color='green') +
  geom_line(aes(x=df$Date,y=df$total_cases,group = 1),color='purple') +
  ylab('Values')+xlab('date')


View(df)
df_temp <-subset(df,select = c(74,75,112,113))
View(df_temp)


acf(df_temp, lag.max = 36)


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


df_acf <- acf(df_temp, lag.max = 36)

plot.acf(df_acf)