rm(list=ls())
library(stringr)
library(ggplot2)

df <- read.csv("final_df2_date_cleaned.csv")
df <- subset(df,select = -c(1))


data <- read.csv("covid_19_india.csv")
data <- subset(data,select = c(2,4,9))
colnames(data) <- c("Date","State","Cases")

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
data$datee <- 0

for(i in 1:length(rownames(data)))
{
  
  s<- as.character(data$Date[i])
  day <- substr(s,9,10)
  month <- substr(s,6,7)
  year <- substr(s,1,4)
  data$datee[i] <-paste(paste(year,substrRight(paste("0",month, sep =""),2), sep =""),substrRight(paste("0",day, sep =""),2), sep ="")
}
data <- subset(data,select = -c(1))
colnames(data) <- c("State","Cases","Date")

#--------------------------------------------------------------------------------------------------------------------------------

df <- df[df$division== "Maharashtra",]
View(df)

temp_df <-data[data$State == "Maharashtra", ]
temp_df<- subset(temp_df,select = -c(1))
View(temp_df)

t <- intersect(df$date,temp_df$Date)
t


df_new <- df[df$date %in% t,]
df_new_freq <- as.data.frame(table(df_new$date))
colnames(df_new_freq) <- c("Date","Strains")
View(df_new_freq)


data_new <- temp_df[temp_df$Date %in% t,]
View(data_new)