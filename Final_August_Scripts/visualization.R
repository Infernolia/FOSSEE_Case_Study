#install.packages("sp")
#install.packages("ggmap")
#install.packages("rgdal")
#install.packages("rgeos")
#install.packages("sf")
#install.packages("mapproj")
#install.packages("magick")
#install.packages("plotfunctions")
rm(list=ls())
library(sp)
library(stringr)
library(ggplot2)
library(sf)
library(ggmap)
library(ggplot2)
library(RColorBrewer)
library(ggmap)
library(maps)
library(rgdal)
library(scales)
library(maptools)
library(gridExtra)
library(rgeos)
library(mapproj)
library(magick)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(plotfunctions)
theme_set(theme_pubr())
source("functions.R")


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


df <- read.csv("data_final/cleaned_data_strains.csv")
df <- subset(df,select = -c(1))




#--Mapping files--#
isro<-st_read("cauvery-INDIA_STATE_250K.kml")
isro<-isro[-1,] # ambiguous 

## Lines below plots Indian map and centroid for each state
plot(st_geometry(isro),axes = TRUE, col = "white")



set.seed(100)



df[df$division=="Andaman and Nicobar Islands", "division"] <- "Andaman and Nicobar"
df[df$division=="Harayana", "division"] <- "Haryana"
df[df$division=="Himachla Pradesh", "division"] <- "Himachal Pradesh"
df[df$division=="Jammu", "division"] <- "Jammu and Kashmir"
df[df$division=="Maharastra", "division"] <- "Maharashtra"
df[df$division=="Odisha", "division"] <- "Orissa"
df[df$division=="Uttarakhand", "division"] <- "Uttaranchal"
df$Datee <- "2000-00-00"
for(i in 1:nrow(df))
{
  df$Datee[i] <-paste(paste(df$year[i],substrRight(paste("0",df$month[i], sep =""),2), sep ="-"),substrRight(paste("0",df$day[i], sep =""),2), sep ="-")
  
}
df$date <- df$Datee
df <- subset(df,select = -c(18))

for(time in unique(df$date))
{
  temp_df <- df[df$date==time,]
  index <- read.csv("data_final/states.csv")
  index$V2 <- 0

  for (i in 1:nrow(index))
  {
    for(j in  1:nrow(temp_df))
    {
      if(index$V1[i]==temp_df$division[j])
      {
        index$V2[i]  <-  index$V2[i] +1;
      }
    }
  }
  
  input_start<-min(index$V2)
  input_end<-max(index$V2)
  output_start<-5
  output_end<-20
  
  
  ## Calculating wavelength based on norm
  color<-NULL
  for(i in 1:length(index$V2)){
    input = index$V2[i]
    output = output_start + ((output_end - output_start) / (input_end - input_start)) * (input - input_start)
    color<-rbind(color,output)
  }
  
  color.rgb<-rainbow(20, rev = T)[color] 

  index.numbers<-as.numeric(index[,2])

  color.index<- color.rgb[index.numbers]
  
  # Plotting the grid
  
  title_string <- paste("Strains Frequency Map at ",time)
  
  s <- paste("plot/",paste0(time,"_map.png"))
  png(s)
  par(mfrow=c(1,1))
  plot(1:100, type ="n", xlim=c(60,110), ylim=c(5,40), xlab="Latitude", ylab="Longitude", main=title_string)
  for(i in 1:36){
    plot(st_geometry(isro[i,]),col =heat.colors(200)[index$V2[i]], add =TRUE)
  }
  gradientLegend(1:200 , color = "heat")
  dev.off()


}




dir_out <- file.path("C:/Users/Aboli/Desktop/FOSSEE/Case Study/code/plot")
print(dir_out)
imgs <- list.files(dir_out, full.names = TRUE)
img_list <- lapply(imgs, image_read)
## join the images together
img_joined <- image_join(img_list)
## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 10)
## view animated image
img_animated

## save to disk
image_write(image = img_animated,path = "gifs/strains_freq_animation2.gif")




