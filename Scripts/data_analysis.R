rm(list=ls())

library(ggplot2)
library(DataExplorer)
library(gridExtra)

#install.packages("stringr")
library(stringr)

df <- read.csv("final_df1.csv")
df <- subset(df,select = -c(1))
View(df)
length(colnames(df))

plot_intro(df)


data.frame(names=df$date,chr=apply(df,2,nchar)[,2])


df$loa <- str_count(df$date, "-")
df$loa

df$year <- 0
df$month <- 0
df$day <- 0

View(df)

a <- 0
b <- 0
c <- 0

for(i in 1:length(rownames(df)))
{
  if(df$loa[i]==2)
  {
    a = a + 1
    df$year[i] <- substr(df$date[i],7,10)
    df$month[i] <- substr(df$date[i],4,5)
    df$day[i] <- substr(df$date[i],1,2)
  }
  else if(df$loa[i]==1)
  {
    b = b+1
    df$year[i] <- substr(df$date[i],1,4)
    df$month[i] <- substr(df$date[i],6,7)
  }
  else
  {
    c = c+1
    df$year[i] <- df$date[i]
  }
}

# Removing the data column as it has different format answers
df <- subset(df,select = -c(3))

View(df)


#table(df$month)[which(df$year==2021)]

df1 <- df[df$year == "2021", ]
df2 <- df1[df1$month == "06", ]
table(df2$day)


# Removing the loa column as we dont need it anymore
df <- subset(df,select = -c(14))



substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

df$date <- 0

for(i in 1:length(rownames(df)))
{
  if(df$day[i]==0)
  {
    
    df$date[i] <-paste(paste(df$year[i],substrRight(paste("0",df$month[i], sep =""),2), sep =""),"00", sep ="")
    
  }
  else if(df$month[i]==0)
  {
    df$date[i] <-paste(paste(df$year[i],"00", sep =""),"00", sep ="")
  }
  else
  {
    df$date[i] <-paste(paste(df$year[i],substrRight(paste("0",df$month[i], sep =""),2), sep =""),substrRight(paste("0",df$day[i], sep =""),2), sep ="")
  }
}

View(df)

barplot(table(df[,c(17)]))

View(df)


write.csv(df,"final_df2.csv")


#1784 rows are removed with missing day or month
df1 <- df[ df$day!=0, ]
df2 <- df1[ df1$month!=0, ]
View(df2)

write.csv(df2,"final_df2_date_cleaned.csv")