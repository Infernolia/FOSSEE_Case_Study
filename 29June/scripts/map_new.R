#install.packages("sp")
#install.packages("ggmap")
#install.packages("rgdal")
#install.packages("rgeos")
#install.packages("sf")
#install.packages("mapproj")
#install.packages("magick")
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


df <- read.csv("final_df2_date_cleaned.csv")
df <- subset(df,select = -c(1))
View(df)



states_shape =readShapeSpatial("IND_adm/IND_adm1.shp")
class(states_shape)
names(states_shape)
print(states_shape$ID_1)
print(states_shape$NAME_1)
plot(states_shape, main = "Administrative Map of India")

set.seed(100)



for(time in unique(df$date))
{
  temp_df <- df[df$date==time,]
  State_count = length(states_shape$NAME_1)
  State_data = data.frame(id=states_shape$ID_1, NAME_1=states_shape$NAME_1)
  State_data$Value <- 0
  for (i in 1:nrow(State_data))
  {
    for(j in  1:nrow(temp_df))
    {
      if(State_data$NAME_1[i]==temp_df$division[j])
      {
        State_data$Value[i]  <-  State_data$Value[i] +1;
      }
    }
  }
  
  fortify_shape = fortify(states_shape, region = "ID_1")
  class(fortify_shape)
  
  Merged_data = merge(fortify_shape, State_data, by="id", all.x=TRUE)
  Map_plot = Merged_data[order(Merged_data$order), ]
  
  
  p <- ggplot() +
    geom_polygon(data = Map_plot,
                 aes(x = long, y = lat, group = group, fill = Value),
                 color = "black", size = 0.5) +
    scale_fill_distiller(palette = "Oranges",trans="reverse",limits = c(100,0))+
    coord_map()
  
  s <- paste0(time,"plot.png")
  png(s)
  print(p)
  dev.off()
  
  
 
}



dir_out <- file.path("C:/Users/Aboli/Desktop/FOSSEE/Case Study/plots")
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
image_write(image = img_animated,path = "strains-animated.gif")


