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


df[2:75] <- as.data.frame(sapply(df[2:75], as.numeric))

for (i in 1:41)
{
      if(i!=6 && i!=9 && i!=11 && i!=35 && i!=38)
      {
          p1 <- paste(paste("State",i,sep=""),"_Confirmed",sep="")
          p2 <- paste(paste("State",i,sep=""),"_Deaths",sep="")
          p3 <- paste(paste("State",i,sep=""),"_Severity",sep="")
          df$p3 <- as.double(0)
          for(j in 1:nrow(df))
          {
              if(df[j,p1]<=0 || df[j,p2]<0)
              {
                 df[j,p3] <- 0.0
              }
              else
              {
                  df[j,p3] <- as.double(df[j,p2])/as.double(df[j,p1])
              }
                
          }
      }
  
}
df <- subset(df,select = -c(76))
df$"Total_severity" <- as.double(0)
for(j in 1:nrow(df))
{
  if(df[j,"total_cases"]<=0 || df[j,"total_deaths"]<0)
  {
    df[j,"Total_severity"] <- 0.0
  }
  else
  {
    df[j,"Total_severity"] <- as.double(df[j,"total_deaths"])/as.double(df[j,"total_cases"])
  }
  
}
View(df)
plot_intro(df)

ncol(df)

for(i in 1:41)
{
  
  if(i!=6 && i!=9 && i!=11 && i!=35 && i!=38)
  {
    s <- paste0(i,"_severity.png")
    p <- paste(paste("State",i,sep=""),"_Severity",sep="")
    t <- ggplot() +   geom_line(aes(x=df[,1],y=df[,p],group = 1),color='blue') + ylab('Values')+xlab('date')
    png(s)
    print(t)
    dev.off()
    
  }
  
}


dir_out <- file.path("C:/Users/Aboli/Desktop/FOSSEE/Case Study/severity")
print(dir_out)
imgs <- list.files(dir_out, full.names = TRUE)
img_list <- lapply(imgs, image_read)
## join the images together
img_joined <- image_join(img_list)
## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 2)
## view animated image
img_animated

## save to disk
image_write(image = img_animated,path = "severity.gif")
