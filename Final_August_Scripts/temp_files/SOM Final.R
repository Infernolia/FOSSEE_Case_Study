#--Libraries required--#

suppressWarnings(library(readr))
suppressWarnings(library(dplyr))
suppressWarnings(library(rgdal))
suppressWarnings(library(sf))
suppressWarnings(library(ggplot2))
suppressWarnings(library(magick))

#--Functions required for SOM--#
source("functions.R")


#--Mapping files--#
isro<-st_read("cauvery-INDIA_STATE_250K.kml")
isro<-isro[-1,] # ambiguous 

    ## Lines below plots Indian map and centroid for each state
    plot(st_geometry(isro),axes = TRUE, col = "white")



#--New COVID Data--#
covid<-read.csv("covid_19_india.csv")
View(covid)

# Taking deaths [8] and conformed cases [9]
data<-covid[,c(2,4,8,9)]


#Note: rows corresponding to dates
#   1) 12207 for march 1
#   2) 14655 for may 8

data<-data[12207:length(data[,1]),] %>% arrange(Date,State.UnionTerritory)
View(data)


# For testing first four weeks individually
## change data.set<-week_1[,-1]

week_1 <- cbind.data.frame(Name = data[1:36,2],
                         Deaths = data[253:288,3]-data[1:36,3]/7,
                         Active = data[253:288,4]-data[1:36,4]/7)

week_2 <- cbind.data.frame(Name = data[37:72,2],
                           Deaths = data[289:324,3]-data[37:72,3]/7,
                           Active = data[289:324,4]-data[37:72,4]/7)
 
week_3 <- cbind.data.frame(Name = data[73:108,2],
                         Deaths = data[325:360,3]-data[73:108,3],
                         Active = data[325:360,4]-data[73:108,4]/7)

week_4 <- cbind.data.frame(Name = data[109:144,2],
                         Deaths = data[361:396,3]-data[109:144,3]/7,
                         Active = data[361:396,4]-data[109:144,4]/7)


average.length<-7
iteration<-0 
date.range<-seq(from = as.Date("2021/3/1"), to = as.Date("2021/6/1"),by = "day")

# Run loop to iterate over all the dates
# Else if only testing for individual weeks run the statements inside the loop without executing the loop

# Note : the loop saves plots as images in "SOM Plots" folder
for(iteration in 0:length(date.range)){
  
  start<-36*iteration+1
  end<-start+36*average.length
  
  previous.range<-seq(start,start+35)
  later.range<-seq(end,end+35)
  
  week<-cbind.data.frame(Name = data[1:36,2],
                         Deaths = (data[later.range,3] - data[previous.range,3])/average.length,
                         Conformed = (data[later.range,4] - data[previous.range,4])/average.length
  )
  

  #Note : change below mentioned line to 
  #       data.set<-week_1[,-1] if running for individual weeks
  
  data.set<-week[,-1]  # removing the names
  
  
  #--Creating the Grid--#
  
  #Creating a 4*4 grid using the function defined above.
  set.seed(222)
  grid <- create_grid(30,30,2)
  
  #--Training the model--# 
  y <- SOM(data.set,grid)
  
  # These are the returned weights for 900 neurons i.e. 30X30 grid
  gridSOM <- y[1]

  
  
  # For saving the plot
  img.name<-paste0("Week ",iteration+1," ",date.range[iteration+1],".png",sep = "")
  png(img.name, width = 1280, height = 720)
  
  par(mfrow =c(1,2))
  
  #--Retrieving the weights and plotting the map--#
  
  gridSOM<-matrix(unlist(gridSOM),ncol=2)
  color.rgb<-drawGrid(gridSOM,c(30,30),TRUE)
  
  #--Indexing showing which state corresponds to which neuron--#
  
  index<-NULL
  
  
  for(i in 1:nrow(data.set)){
    k<-as.matrix(data.set[i,],ncol = 2)
    index<-rbind(index,c(week[i,1],BMU_Vectorised.2(k,gridSOM)))
  }
  
  # removing states which are not present in the map
  index<-rbind(index[1:8,],index[8,],index[9:36,])
  index<-index[-c(19,33),]
  
  # to view indexing un-comment the line below and run
  #index
  
  
  index.numbers<-as.numeric(index[,2])
  color.index<-color.rgb[index.numbers]
  
  # Plotting the grid
  
  plot(1:100, type ="n", xlim=c(60,110), ylim=c(5,40), xlab="Latitude", ylab="Longitude", main="India")
  for(i in 1:36){
    plot(st_geometry(isro[i,]),col =color.index[i], add =TRUE)
  }
  
  dev.off()
}


View(color.index[1])

write.csv(index,"states.csv")