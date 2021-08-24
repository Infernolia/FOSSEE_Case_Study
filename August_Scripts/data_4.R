rm(list=ls())
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



ggplot(data, aes(datee,total_cases)) +
  geom_bar(stat = "summary", fun.y = "mean")  + 
  ylab('Cases')

t <- intersect(df$date,data$datee)
t

df_new <- df[df$date %in% t,]
df_new_freq <- as.data.frame(table(df_new$date))
colnames(df_new_freq) <- c("Date","Strains")


data_new <- data[data$datee %in% t,]


data_new$strains <- 0
data_new$clade <- ""
data_new$lineage <- ""

for(i in 1:length(rownames(data_new)))
{
  day <- data_new$datee[i]
  temp_df <- df_new_freq[df_new_freq$Date == day, ]
  temp_df1 <-df[df$date == day, ]
  data_new$strains[i] <- min(temp_df$Strains)
  if(nrow(temp_df1)!=0)
  {
  data_new$clade[i] <- temp_df1$GISAID_clade[1]
  data_new$lineage[i] <- temp_df1$pangolin_lineage[1]
  }
  else
  {
    data_new$clade[i] <- "Ab"
    data_new$lineage[i] <-"Bb"
  }
}  

View(data_new)



data_new$total_cases <- scale(data_new$total_cases)
data_new$strains <- scale(data_new$strains)
data_new$clade <-as.numeric(factor(data_new$clade))
data_new$clade <-scale(data_new$clade)
data_new$lineage <-as.numeric(factor(data_new$lineage))
data_new$lineage <-scale(data_new$lineage)

ggplot() + 
  geom_line(aes(x=data_new$datee,y=data_new$strains,group = 1),color='blue') + 
  ylab('Values')+xlab('date')

ggplot() + 
geom_line(aes(x=data_new$datee,y=data_new$total_cases,group = 1),color='red') +
  ylab('Values')+xlab('date')


table(data_new$clade)
ggplot() + 
  geom_line(aes(x=data_new$datee,y=data_new$strains,group = 1),color='blue') + 
  geom_line(aes(x=data_new$datee,y=data_new$total_cases,group = 1),color='red') +
  ylab('Values')+xlab('date')


ggplot() + 
  geom_line(aes(x=data_new$datee,y=data_new$clade,group = 1),color='blue') + 
  ylab('Values')+xlab('date')


ggplot() + 
  geom_line(aes(x=data_new$datee,y=data_new$clade,group = 1),color='blue') + 
  geom_line(aes(x=data_new$datee,y=data_new$total_cases,group = 1),color='red') +
  ylab('Values')+xlab('date')


ggplot() + 
  geom_line(aes(x=data_new$datee,y=data_new$lineage,group = 1),color='blue') + 
  ylab('Values')+xlab('date')


ggplot() + 
  geom_line(aes(x=data_new$datee,y=data_new$lineage,group = 1),color='blue') + 
  geom_line(aes(x=data_new$datee,y=data_new$total_cases,group = 1),color='red') +
  ylab('Values')+xlab('date')

View(df[df$host =="Environment",])