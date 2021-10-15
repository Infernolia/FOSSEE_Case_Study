rm(list=ls())
# Need the "rgdal", "rgeos","maps","sp" and "mapproj" packages for "sf" package to run.
#install.packages("rgdal")
#install.packages("rgeos")
#install.packages("sp")
#install.packages("map")
#install.packages("mapproj")
library(rgdal)
library(rgeos)
library(mapproj)
library(maps)
library(sp)
# Need the "sf" package for "st_read" and "st_geometry" functions.
#install.packages("sf")
library(sf)

# Need the "plotfunctions" package for "gradientLegend" function.
#install.packages("plotfunctions")
library(plotfunctions)


# Need the "magick" package for "image_write","image_animate","image_join" functions.
#install.packages("magick")
library(magick)


#----------------------------------------------------------------------------------------------------
# Visualizing the cleaned COVID-19 Cases data
#----------------------------------------------------------------------------------------------------
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}



df <- read.csv("data_final/cleaned_data_cases.csv")
df <- subset(df,select = -c(1))


i = "Maharashtra"

temp <- df[df$State.UnionTerritory == i, ] 
for (t in 1:(nrow(temp)))
{
  temp$year[t] <- substr(temp$Date[t],7,10)
  temp$month[t] <- substr(temp$Date[t],4,5)
  temp$day[t] <- substr(temp$Date[t],1,2)
  temp$date[t] <-paste(paste(temp$year[t],substrRight(paste("0",temp$month[t], sep =""),2), sep =""),substrRight(paste("0",temp$day[t], sep =""),2), sep ="")
}


plot(x=temp$date,y=temp$Confirmed,col='blue',lty=1,xlab="Date",ylab="Values") 
lines(temp$date, temp$Cured, col="red",lty=1)
lines(temp$date, temp$Deaths, col="green", lty=1)
legend("bottomright", c("Confirmed Cases", "Cured","Deaths"),lty = c(1,1,1), lwd = 2, col = c("blue","red","green"), bty = "n")




#-----------------------------------------------------------------------------------
# Plotting on same axis
#-----------------------------------------------------------------------------------
temp$x <- 0
for(i in 1:nrow(temp)) {  temp$x[i] <- i}


plot(x=temp$x,y=temp$Confirmed,col='blue',lty=1,xlab="Date",ylab="Values") 
lines(temp$x, temp$Cured, col="red",lty=1)
lines(temp$x, temp$Deaths, col="green", lty=1)
legend("bottomright", c("Confirmed Cases", "Cured","Deaths"),lty = c(1,1,1), lwd = 2, col = c("blue","red","green"), bty = "n")


par(mar = c(5, 4, 4, 4) + 0.3)              # Additional space for second y-axis
plot(temp$x, temp$Confirmed, pch = 16, col = 2,xlab="Time",ylab="Total Cases")              # Create first plot
par(new = TRUE)                             # Add new plot
plot(temp$x, temp$Deaths, pch = 17, col = 3,              # Create second plot without axes
     axes = FALSE, xlab = "", ylab = "")
axis(side = 4, at = pretty(range(temp$total_deaths)))      # Add second axis
mtext("Total Deaths", side = 4, line = 3)             # Add second axis label




#----------------------------------------------------------------------------------------------------
# Visualizing the cleaned COVID-19 Strains data
#----------------------------------------------------------------------------------------------------

df <- read.csv("data_final/cleaned_data_strains.csv")
df <- subset(df,select = -c(1))



for(i in 1:length(colnames(df)))
{
  barplot(table(df[,c(i)]),xlab=colnames(df)[i],ylab="Frequency")
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}




dir.create("plot")


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
  
  
  # Plotting the grid
  
  title_string <- paste("Strains Frequency Map at ",time)
  
  s <- paste("plot/",paste0(time,"_map.png"))
  png(s)
  par(mfrow=c(1,1))
  plot(1:100, type ="n", xlim=c(60,110), ylim=c(5,40), xlab="Latitude", ylab="Longitude", main=title_string)
  for(i in 1:36){
    plot(st_geometry(isro[i,]),col = topo.colors(170)[index$V2[i]], add =TRUE)
  }
  gradientLegend(1:170,color = "topo", pos=.5, side=4,inside=TRUE)
  dev.off()
  
}




dir_out <- file.path(paste(getwd(),"/plot",sep=""))
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
image_write(image = img_animated,path = "strains_animation.gif")

rm(list=ls())









