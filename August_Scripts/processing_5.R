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
library(ggpubr)
theme_set(theme_pubr())



substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


df <- read.csv("final_df2_date_cleaned.csv")
df <- subset(df,select = -c(1))


states_shape =readShapeSpatial("IND_adm/IND_adm1.shp")
class(states_shape)
names(states_shape)
print(states_shape$ID_1)
print(states_shape$NAME_1)
plot(states_shape, main = "Administrative Map of India")

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
View(df)



#df <- df[df$GISAID_clade=="G",]

# yy <- 0
# for(time in unique(df$date))
# {
#   temp_df <- df[df$date==time,]
#   State_count = length(states_shape$NAME_1)
#   State_data = data.frame(id=states_shape$ID_1, NAME_1=states_shape$NAME_1)
#   State_data$Value <- 0
#   for (i in 1:nrow(State_data))
#   {
#     for(j in  1:nrow(temp_df))
#     {
#       if(State_data$NAME_1[i]==temp_df$division[j])
#       {
#         State_data$Value[i]  <-  State_data$Value[i] +1;
#       }
#     }
#   }
# 
# y <- max(State_data$Value)
# if(y>yy)
# {
#   yy <- y
# }
# 
# 
# 
# }
# 
# print(yy)


# 
# for(time in unique(df$date))
# {
#   temp_df <- df[df$date==time,]
#   
#   
#   title_string <- paste("Clade Frequency Histogram ",time)
#   p <- ggplot(temp_df, aes(temp_df$GISAID_clade)) +
#     geom_bar(fill = "#0073C2FF") +  labs(y= "Frequency", x = "Clade") + ggtitle(title_string)+
#     theme_pubclean()
#   s <- paste0(time,"_clade.png")
#   png(s)
#   print(p)
#   dev.off()
#   
#   
#   
# }



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

  title_string <- paste("Strains Frequency Map at ",time)
  p <- ggplot() +
    geom_polygon(data = Map_plot,
                 aes(x = long, y = lat, group = group, fill = Value),
                 color = "black", size = 0.5) +
    scale_fill_distiller(palette = "Spectral", direction=1,trans="reverse",limits = c(170,0))+
    ggtitle(title_string)+
    coord_map()

  s <- paste0(time,"_map.png")
  png(s)
  print(p)
  dev.off()



}




dir_out <- file.path("C:/Users/Aboli/Desktop/FOSSEE/Case Study/plot3")
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
image_write(image = img_animated,path = "strains_freq_animation1.gif")


