rm(list=ls())
library(stringr)
library(ggplot2)

df <- read.csv("final_df2_date_cleaned.csv")
df <- subset(df,select = -c(1))
View(df)

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

View(data)

ggplot(data, aes(datee,total_cases)) +
  geom_bar(stat = "summary", fun.y = "mean")  + 
  ylab('Cases')

t <- intersect(df$date,data$datee)
t

df_new <- df[df$date %in% t,]
df_new_freq <- as.data.frame(table(df_new$date))
colnames(df_new_freq) <- c("Date","Strains")
View(df_new_freq)

data_new <- data[data$datee %in% t,]
View(data_new)

data_new$strains <- 0

for(i in 1:length(rownames(data_new)))
{
  day <- data_new$datee[i]
  print(day)
  temp_df <- df_new_freq[df_new_freq$Date == day, ]
  data_new$strains[i] <- min(temp_df$Strains)
  
}  

View(data_new)



data_new$total_cases <- scale(data_new$total_cases)
data_new$strains <- scale(data_new$strains)

ggplot() + 
  geom_line(aes(x=data_new$datee,y=data_new$strains,group = 1),color='blue') + 
  ylab('Values')+xlab('date')

ggplot() + 
geom_line(aes(x=data_new$datee,y=data_new$total_cases,group = 1),color='red') +
  ylab('Values')+xlab('date')



ggplot() + 
  geom_line(aes(x=data_new$datee,y=data_new$strains,group = 1),color='blue') + 
  geom_line(aes(x=data_new$datee,y=data_new$total_cases,group = 1),color='red') +
  ylab('Values')+xlab('date')


View(df[df$host =="Environment",])