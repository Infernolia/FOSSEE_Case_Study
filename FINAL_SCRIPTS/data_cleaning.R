rm(list=ls())

#install.packages("ggplot2")
#install.packages("DataExplorer")
#install.packages("gridExtra")
#install.packages("stringr")

library(ggplot2)
library(DataExplorer)
library(gridExtra)
library(stringr)
library(stringr)
library(ggplot2)
library(DataExplorer)
library(stringr)
library(ggplot2)
library(DataExplorer)
library(tseries)
library("TTR")

df <- read.csv("data_final/input_data_strains.csv")
colnames(df)

length(rownames(df))

df$age <- gsub("1 month", "0", df$age)
df$age <- gsub("3 months", "0", df$age)
df$age <- gsub("5 months", "0", df$age)
df$age <- gsub("6 Months", "1", df$age)
df$age <- gsub("9 months", "1", df$age)
df$age <- gsub("7 days", "0", df$age)
df$age <- gsub("unknown", "unknown", df$age)
df$age <- gsub("Unknown", "unknown", df$age)
df$age <- gsub("unknwon", "unknown", df$age)
df$age <- gsub("Unkown", "unknown", df$age)
df$age <- gsub("-", "unknown", df$age)

for(i in 1:length(rownames(df)))
{
  if(df$age[i]=="Female" || df$age[i]=="M" || df$age[i]=="Male" )
  {
    gen <- df$age[i]
    ag <- df$sex[i]
    
    df$age[i] = ag
    df$sex[i] = gen
    
  }
}

table(df$"age")



df$sex <- gsub("FemaleeMaleale", "F", df$sex)
df$sex <- gsub("Female", "F", df$sex)
df$sex <- gsub("FFemale", "F", df$sex)
df$sex <- gsub("Maleale", "M", df$sex)
df$sex <- gsub("Male", "M", df$sex)
df$sex <- gsub("0", "unknown", df$sex)
df$sex <- gsub("-", "unknown", df$sex)
df$sex <- gsub("unknown", "unknown", df$sex)
df$sex <- gsub("Unknown", "unknown", df$sex)
df$sex <- gsub("unknwon", "unknown", df$sex)
df$sex <- gsub("Unkown", "unknown", df$sex)
df$sex <- gsub("FF", "F", df$sex)
df$sex <- gsub("O", "unknown", df$sex)
table(df$"sex")


# Removing missing columns with ? entries
df <- subset(df,select = -c(1,2,4, 6,8,9,12,13, 15, 20,25,26,27,28,29,30))

#Columns removed are garbage index, index, virus(betacoronavirus for all), genbank_accession, region, country, region_exposure, country_exposure, segment, Nextstrain_clade, authors, url, title, paper_url, date submitted to GISAID, purpose_of_sequencing



df$loa <- str_count(df$date, "-")
df$loa

df$year <- 0
df$month <- 0
df$day <- 0

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





#1784 rows are removed with missing day or month
df1 <- df[ df$day!=0, ]
df2 <- df1[ df1$month!=0, ]

strains <- df2
rm(list=setdiff(ls(),"strains"))

#--------------------------------------------------------------------------------------
# Done with initial preprocessing of strains data
# Starting the processing of COVID-19 India cases
#--------------------------------------------------------------------------------------


df <- read.csv("data_final/input_data_cases.csv")
df <- subset(df,select = -c(1))
df <- subset(df,select = c(1,3,7,8))
colnames(df) <- c("Date","State","Deaths","Confirmed")

# We don't need df6, df9, df11, df35, df38 as they either have too few rows or are incorrect states

df1 <- df[df$State=="Andaman and Nicobar Islands",]
df2 <- df[df$State=="Andhra Pradesh",]
df3 <- df[df$State=="Arunachal Pradesh",]
df4 <- df[df$State=="Assam",]
df5 <- df[df$State=="Bihar",]
df6 <- df[df$State=="Cases being reassigned to states",]
df7 <- df[df$State=="Chandigarh",]
df8 <- df[df$State=="Chhattisgarh",]
df9 <- df[df$State=="Dadra and Nagar Haveli",]
df10 <- df[df$State=="Dadra and Nagar Haveli and Daman and Diu",]
df11 <- df[df$State=="Daman & Diu",]
df12 <- df[df$State=="Delhi",]
df13 <- df[df$State=="Goa",]
df14 <- df[df$State=="Gujarat",]
df15 <- df[df$State=="Haryana",]
df16 <- df[df$State=="Himachal Pradesh",]
df17 <- df[df$State=="Jammu and Kashmir",]
df18 <- df[df$State=="Jharkhand",]
df19 <- df[df$State=="Karnataka",]
df20 <- df[df$State=="Kerala",]
df21 <- df[df$State=="Ladakh",]
df22 <- df[df$State=="Lakshadweep",]
df23 <- df[df$State=="Madhya Pradesh",]
df24 <- df[df$State=="Maharashtra",]
df25 <- df[df$State=="Manipur",]
df26 <- df[df$State=="Meghalaya",]
df27 <- df[df$State=="Mizoram",]
df28 <- df[df$State=="Nagaland",]
df29 <- df[df$State=="Odisha",]
df30 <- df[df$State=="Puducherry",]
df31 <- df[df$State=="Punjab",]
df32 <- df[df$State=="Rajasthan",]
df33 <- df[df$State=="Sikkim",]
df34 <- df[df$State=="Tamil Nadu",]
df35 <- df[df$State=="Telangana",]
df36 <- df[df$State=="Telengana",]
df37 <- df[df$State=="Tripura",]
df38 <- df[df$State=="Unassigned",]
df39 <- df[df$State=="Uttar Pradesh",]
df40 <- df[df$State=="Uttarakhand",]
df41 <- df[df$State=="West Bengal",]


df1$D <- 0;df1$C <- 0;df1$D[1] <-  df1$Deaths[1];df1$C[1] <-  df1$Confirmed[1];i <- nrow(df1)
while(i!=1) {    df1$D[i] <- df1$Deaths[i] -df1$Deaths[i-1];df1$C[i] <- df1$Confirmed[i] -df1$Confirmed[i-1]; i <- i-1}
df1 <- subset(df1,select = -c(3,4))

df2$D <- 0;df2$C <- 0;df2$D[1] <-  df2$Deaths[1];df2$C[1] <-  df2$Confirmed[1];i <- nrow(df2)
while(i!=1) {    df2$D[i] <- df2$Deaths[i] -df2$Deaths[i-1];df2$C[i] <- df2$Confirmed[i] -df2$Confirmed[i-1]; i <- i-1}
df2 <- subset(df2,select = -c(3,4))

df3$D <- 0;df3$C <- 0;df3$D[1] <-  df3$Deaths[1];df3$C[1] <-  df3$Confirmed[1];i <- nrow(df3)
while(i!=1) {    df3$D[i] <- df3$Deaths[i] -df3$Deaths[i-1];df3$C[i] <- df3$Confirmed[i] -df3$Confirmed[i-1]; i <- i-1}
df3 <- subset(df3,select = -c(3,4))

df4$D <- 0;df4$C <- 0;df4$D[1] <-  df4$Deaths[1];df4$C[1] <-  df4$Confirmed[1];i <- nrow(df4)
while(i!=1) {    df4$D[i] <- df4$Deaths[i] -df4$Deaths[i-1];df4$C[i] <- df4$Confirmed[i] -df4$Confirmed[i-1]; i <- i-1}
df4 <- subset(df4,select = -c(3,4))

df5$D <- 0;df5$C <- 0;df5$D[1] <-  df5$Deaths[1];df5$C[1] <-  df5$Confirmed[1];i <- nrow(df5)
while(i!=1) {    df5$D[i] <- df5$Deaths[i] -df5$Deaths[i-1];df5$C[i] <- df5$Confirmed[i] -df5$Confirmed[i-1]; i <- i-1}
df5 <- subset(df5,select = -c(3,4))


df6$D <- 0;df6$C <- 0;df6$D[1] <-  df6$Deaths[1];df6$C[1] <-  df6$Confirmed[1];i <- nrow(df6)
while(i!=1) {    df6$D[i] <- df6$Deaths[i] -df6$Deaths[i-1];df6$C[i] <- df6$Confirmed[i] -df6$Confirmed[i-1]; i <- i-1}
df6 <- subset(df6,select = -c(3,4))

df7$D <- 0;df7$C <- 0;df7$D[1] <-  df7$Deaths[1];df7$C[1] <-  df7$Confirmed[1];i <- nrow(df7)
while(i!=1) {    df7$D[i] <- df7$Deaths[i] -df7$Deaths[i-1];df7$C[i] <- df7$Confirmed[i] -df7$Confirmed[i-1]; i <- i-1}
df7 <- subset(df7,select = -c(3,4))

df8$D <- 0;df8$C <- 0;df8$D[1] <-  df8$Deaths[1];df8$C[1] <-  df8$Confirmed[1];i <- nrow(df8)
while(i!=1) {    df8$D[i] <- df8$Deaths[i] -df8$Deaths[i-1];df8$C[i] <- df8$Confirmed[i] -df8$Confirmed[i-1]; i <- i-1}
df8 <- subset(df8,select = -c(3,4))

df9$D <- 0;df9$C <- 0;df9$D[1] <-  df9$Deaths[1];df9$C[1] <-  df9$Confirmed[1];i <- nrow(df9)
while(i!=1) {    df9$D[i] <- df9$Deaths[i] -df9$Deaths[i-1];df9$C[i] <- df9$Confirmed[i] -df9$Confirmed[i-1]; i <- i-1}
df9 <- subset(df9,select = -c(3,4))

df10$D <- 0;df10$C <- 0;df10$D[1] <-  df10$Deaths[1];df10$C[1] <-  df10$Confirmed[1];i <- nrow(df10)
while(i!=1) {    df10$D[i] <- df10$Deaths[i] -df10$Deaths[i-1];df10$C[i] <- df10$Confirmed[i] -df10$Confirmed[i-1]; i <- i-1}
df10 <- subset(df10,select = -c(3,4))


df11$D <- 0;df11$C <- 0;df11$D[1] <-  df11$Deaths[1];df11$C[1] <-  df11$Confirmed[1];i <- nrow(df11)
while(i!=1) {    df11$D[i] <- df11$Deaths[i] -df11$Deaths[i-1];df11$C[i] <- df11$Confirmed[i] -df11$Confirmed[i-1]; i <- i-1}
df11 <- subset(df11,select = -c(3,4))

df12$D <- 0;df12$C <- 0;df12$D[1] <-  df12$Deaths[1];df12$C[1] <-  df12$Confirmed[1];i <- nrow(df12)
while(i!=1) {    df12$D[i] <- df12$Deaths[i] -df12$Deaths[i-1];df12$C[i] <- df12$Confirmed[i] -df12$Confirmed[i-1]; i <- i-1}
df12 <- subset(df12,select = -c(3,4))

df13$D <- 0;df13$C <- 0;df13$D[1] <-  df13$Deaths[1];df13$C[1] <-  df13$Confirmed[1];i <- nrow(df13)
while(i!=1) {    df13$D[i] <- df13$Deaths[i] -df13$Deaths[i-1];df13$C[i] <- df13$Confirmed[i] -df13$Confirmed[i-1]; i <- i-1}
df13 <- subset(df13,select = -c(3,4))

df14$D <- 0;df14$C <- 0;df14$D[1] <-  df14$Deaths[1];df14$C[1] <-  df14$Confirmed[1];i <- nrow(df14)
while(i!=1) {    df14$D[i] <- df14$Deaths[i] -df14$Deaths[i-1];df14$C[i] <- df14$Confirmed[i] -df14$Confirmed[i-1]; i <- i-1}
df14 <- subset(df14,select = -c(3,4))

df15$D <- 0;df15$C <- 0;df15$D[1] <-  df15$Deaths[1];df15$C[1] <-  df15$Confirmed[1];i <- nrow(df15)
while(i!=1) {    df15$D[i] <- df15$Deaths[i] -df15$Deaths[i-1];df15$C[i] <- df15$Confirmed[i] -df15$Confirmed[i-1]; i <- i-1}
df15 <- subset(df15,select = -c(3,4))

df16$D <- 0;df16$C <- 0;df16$D[1] <-  df16$Deaths[1];df16$C[1] <-  df16$Confirmed[1];i <- nrow(df16)
while(i!=1) {    df16$D[i] <- df16$Deaths[i] -df16$Deaths[i-1];df16$C[i] <- df16$Confirmed[i] -df16$Confirmed[i-1]; i <- i-1}
df16 <- subset(df16,select = -c(3,4))

df17$D <- 0;df17$C <- 0;df17$D[1] <-  df17$Deaths[1];df17$C[1] <-  df17$Confirmed[1];i <- nrow(df17)
while(i!=1) {    df17$D[i] <- df17$Deaths[i] -df17$Deaths[i-1];df17$C[i] <- df17$Confirmed[i] -df17$Confirmed[i-1]; i <- i-1}
df17 <- subset(df17,select = -c(3,4))

df18$D <- 0;df18$C <- 0;df18$D[1] <-  df18$Deaths[1];df18$C[1] <-  df18$Confirmed[1];i <- nrow(df18)
while(i!=1) {    df18$D[i] <- df18$Deaths[i] -df18$Deaths[i-1];df18$C[i] <- df18$Confirmed[i] -df18$Confirmed[i-1]; i <- i-1}
df18 <- subset(df18,select = -c(3,4))

df19$D <- 0;df19$C <- 0;df19$D[1] <-  df19$Deaths[1];df19$C[1] <-  df19$Confirmed[1];i <- nrow(df19)
while(i!=1) {    df19$D[i] <- df19$Deaths[i] -df19$Deaths[i-1];df19$C[i] <- df19$Confirmed[i] -df19$Confirmed[i-1]; i <- i-1}
df19 <- subset(df19,select = -c(3,4))

df20$D <- 0;df20$C <- 0;df20$D[1] <-  df20$Deaths[1];df20$C[1] <-  df20$Confirmed[1];i <- nrow(df20)
while(i!=1) {    df20$D[i] <- df20$Deaths[i] -df20$Deaths[i-1];df20$C[i] <- df20$Confirmed[i] -df20$Confirmed[i-1]; i <- i-1}
df20 <- subset(df20,select = -c(3,4))

df21$D <- 0;df21$C <- 0;df21$D[1] <-  df21$Deaths[1];df21$C[1] <-  df21$Confirmed[1];i <- nrow(df21)
while(i!=1) {    df21$D[i] <- df21$Deaths[i] -df21$Deaths[i-1];df21$C[i] <- df21$Confirmed[i] -df21$Confirmed[i-1]; i <- i-1}
df21 <- subset(df21,select = -c(3,4))

df22$D <- 0;df22$C <- 0;df22$D[1] <-  df22$Deaths[1];df22$C[1] <-  df22$Confirmed[1];i <- nrow(df22)
while(i!=1) {    df22$D[i] <- df22$Deaths[i] -df22$Deaths[i-1];df22$C[i] <- df22$Confirmed[i] -df22$Confirmed[i-1]; i <- i-1}
df22 <- subset(df22,select = -c(3,4))

df23$D <- 0;df23$C <- 0;df23$D[1] <-  df23$Deaths[1];df23$C[1] <-  df23$Confirmed[1];i <- nrow(df23)
while(i!=1) {    df23$D[i] <- df23$Deaths[i] -df23$Deaths[i-1];df23$C[i] <- df23$Confirmed[i] -df23$Confirmed[i-1]; i <- i-1}
df23 <- subset(df23,select = -c(3,4))

df24$D <- 0;df24$C <- 0;df24$D[1] <-  df24$Deaths[1];df24$C[1] <-  df24$Confirmed[1];i <- nrow(df24)
while(i!=1) {    df24$D[i] <- df24$Deaths[i] -df24$Deaths[i-1];df24$C[i] <- df24$Confirmed[i] -df24$Confirmed[i-1]; i <- i-1}
df24 <- subset(df24,select = -c(3,4))

df25$D <- 0;df25$C <- 0;df25$D[1] <-  df25$Deaths[1];df25$C[1] <-  df25$Confirmed[1];i <- nrow(df25)
while(i!=1) {    df25$D[i] <- df25$Deaths[i] -df25$Deaths[i-1];df25$C[i] <- df25$Confirmed[i] -df25$Confirmed[i-1]; i <- i-1}
df25 <- subset(df25,select = -c(3,4))

df26$D <- 0;df26$C <- 0;df26$D[1] <-  df26$Deaths[1];df26$C[1] <-  df26$Confirmed[1];i <- nrow(df26)
while(i!=1) {    df26$D[i] <- df26$Deaths[i] -df26$Deaths[i-1];df26$C[i] <- df26$Confirmed[i] -df26$Confirmed[i-1]; i <- i-1}
df26 <- subset(df26,select = -c(3,4))

df27$D <- 0;df27$C <- 0;df27$D[1] <-  df27$Deaths[1];df27$C[1] <-  df27$Confirmed[1];i <- nrow(df27)
while(i!=1) {    df27$D[i] <- df27$Deaths[i] -df27$Deaths[i-1];df27$C[i] <- df27$Confirmed[i] -df27$Confirmed[i-1]; i <- i-1}
df27 <- subset(df27,select = -c(3,4))

df28$D <- 0;df28$C <- 0;df28$D[1] <-  df28$Deaths[1];df28$C[1] <-  df28$Confirmed[1];i <- nrow(df28)
while(i!=1) {    df28$D[i] <- df28$Deaths[i] -df28$Deaths[i-1];df28$C[i] <- df28$Confirmed[i] -df28$Confirmed[i-1]; i <- i-1}
df28 <- subset(df28,select = -c(3,4))

df29$D <- 0;df29$C <- 0;df29$D[1] <-  df29$Deaths[1];df29$C[1] <-  df29$Confirmed[1];i <- nrow(df29)
while(i!=1) {    df29$D[i] <- df29$Deaths[i] -df29$Deaths[i-1];df29$C[i] <- df29$Confirmed[i] -df29$Confirmed[i-1]; i <- i-1}
df29 <- subset(df29,select = -c(3,4))

df30$D <- 0;df30$C <- 0;df30$D[1] <-  df30$Deaths[1];df30$C[1] <-  df30$Confirmed[1];i <- nrow(df30)
while(i!=1) {    df30$D[i] <- df30$Deaths[i] -df30$Deaths[i-1];df30$C[i] <- df30$Confirmed[i] -df30$Confirmed[i-1]; i <- i-1}
df30 <- subset(df30,select = -c(3,4))

df31$D <- 0;df31$C <- 0;df31$D[1] <-  df31$Deaths[1];df31$C[1] <-  df31$Confirmed[1];i <- nrow(df31)
while(i!=1) {    df31$D[i] <- df31$Deaths[i] -df31$Deaths[i-1];df31$C[i] <- df31$Confirmed[i] -df31$Confirmed[i-1]; i <- i-1}
df31 <- subset(df31,select = -c(3,4))

df32$D <- 0;df32$C <- 0;df32$D[1] <-  df32$Deaths[1];df32$C[1] <-  df32$Confirmed[1];i <- nrow(df32)
while(i!=1) {    df32$D[i] <- df32$Deaths[i] -df32$Deaths[i-1];df32$C[i] <- df32$Confirmed[i] -df32$Confirmed[i-1]; i <- i-1}
df32 <- subset(df32,select = -c(3,4))

df33$D <- 0;df33$C <- 0;df33$D[1] <-  df33$Deaths[1];df33$C[1] <-  df33$Confirmed[1];i <- nrow(df33)
while(i!=1) {    df33$D[i] <- df33$Deaths[i] -df33$Deaths[i-1];df33$C[i] <- df33$Confirmed[i] -df33$Confirmed[i-1]; i <- i-1}
df33 <- subset(df33,select = -c(3,4))

df34$D <- 0;df34$C <- 0;df34$D[1] <-  df34$Deaths[1];df34$C[1] <-  df34$Confirmed[1];i <- nrow(df34)
while(i!=1) {    df34$D[i] <- df34$Deaths[i] -df34$Deaths[i-1];df34$C[i] <- df34$Confirmed[i] -df34$Confirmed[i-1]; i <- i-1}
df34 <- subset(df34,select = -c(3,4))

df35$D <- 0;df35$C <- 0;df35$D[1] <-  df35$Deaths[1];df35$C[1] <-  df35$Confirmed[1];i <- nrow(df35)
while(i!=1) {    df35$D[i] <- df35$Deaths[i] -df35$Deaths[i-1];df35$C[i] <- df35$Confirmed[i] -df35$Confirmed[i-1]; i <- i-1}
df35 <- subset(df35,select = -c(3,4))

df36$D <- 0;df36$C <- 0;df36$D[1] <-  df36$Deaths[1];df36$C[1] <-  df36$Confirmed[1];i <- nrow(df36)
while(i!=1) {    df36$D[i] <- df36$Deaths[i] -df36$Deaths[i-1];df36$C[i] <- df36$Confirmed[i] -df36$Confirmed[i-1]; i <- i-1}
df36 <- subset(df36,select = -c(3,4))

df37$D <- 0;df37$C <- 0;df37$D[1] <-  df37$Deaths[1];df37$C[1] <-  df37$Confirmed[1];i <- nrow(df37)
while(i!=1) {    df37$D[i] <- df37$Deaths[i] -df37$Deaths[i-1];df37$C[i] <- df37$Confirmed[i] -df37$Confirmed[i-1]; i <- i-1}
df37 <- subset(df37,select = -c(3,4))

df38$D <- 0;df38$C <- 0;df38$D[1] <-  df38$Deaths[1];df38$C[1] <-  df38$Confirmed[1];i <- nrow(df38)
while(i!=1) {    df38$D[i] <- df38$Deaths[i] -df38$Deaths[i-1];df38$C[i] <- df38$Confirmed[i] -df38$Confirmed[i-1]; i <- i-1}
df38 <- subset(df38,select = -c(3,4))

df39$D <- 0;df39$C <- 0;df39$D[1] <-  df39$Deaths[1];df39$C[1] <-  df39$Confirmed[1];i <- nrow(df39)
while(i!=1) {    df39$D[i] <- df39$Deaths[i] -df39$Deaths[i-1];df39$C[i] <- df39$Confirmed[i] -df39$Confirmed[i-1]; i <- i-1}
df39 <- subset(df39,select = -c(3,4))

df40$D <- 0;df40$C <- 0;df40$D[1] <-  df40$Deaths[1];df40$C[1] <-  df40$Confirmed[1];i <- nrow(df40)
while(i!=1) {    df40$D[i] <- df40$Deaths[i] -df40$Deaths[i-1];df40$C[i] <- df40$Confirmed[i] -df40$Confirmed[i-1]; i <- i-1}
df40 <- subset(df40,select = -c(3,4))

df41$D <- 0;df41$C <- 0;df41$D[1] <-  df41$Deaths[1];df41$C[1] <-  df41$Confirmed[1];i <- nrow(df41)
while(i!=1) {    df41$D[i] <- df41$Deaths[i] -df41$Deaths[i-1];df41$C[i] <- df41$Confirmed[i] -df41$Confirmed[i-1]; i <- i-1}
df41 <- subset(df41,select = -c(3,4))




colnames(df1) <- c("Date","State","State1_Deaths", "State1_Confirmed")
colnames(df2) <- c("Date","State","State2_Deaths", "State2_Confirmed")
colnames(df3) <- c("Date","State","State3_Deaths", "State3_Confirmed")
colnames(df4) <- c("Date","State","State4_Deaths", "State4_Confirmed")
colnames(df5) <- c("Date","State","State5_Deaths", "State5_Confirmed")
colnames(df6) <- c("Date","State","State6_Deaths", "State6_Confirmed")
colnames(df7) <- c("Date","State","State7_Deaths", "State7_Confirmed")
colnames(df8) <- c("Date","State","State8_Deaths", "State8_Confirmed")
colnames(df9) <- c("Date","State","State9_Deaths", "State9_Confirmed")
colnames(df10) <- c("Date","State","State10_Deaths", "State10_Confirmed")
colnames(df11) <- c("Date","State","State11_Deaths", "State11_Confirmed")
colnames(df12) <- c("Date","State","State12_Deaths", "State12_Confirmed")
colnames(df13) <- c("Date","State","State13_Deaths", "State13_Confirmed")
colnames(df14) <- c("Date","State","State14_Deaths", "State14_Confirmed")
colnames(df15) <- c("Date","State","State15_Deaths", "State15_Confirmed")
colnames(df16) <- c("Date","State","State16_Deaths", "State16_Confirmed")
colnames(df17) <- c("Date","State","State17_Deaths", "State17_Confirmed")
colnames(df18) <- c("Date","State","State18_Deaths", "State18_Confirmed")
colnames(df19) <- c("Date","State","State19_Deaths", "State19_Confirmed")
colnames(df20) <- c("Date","State","State20_Deaths", "State20_Confirmed")
colnames(df21) <- c("Date","State","State21_Deaths", "State21_Confirmed")
colnames(df22) <- c("Date","State","State22_Deaths", "State22_Confirmed")
colnames(df23) <- c("Date","State","State23_Deaths", "State23_Confirmed")
colnames(df24) <- c("Date","State","State24_Deaths", "State24_Confirmed")
colnames(df25) <- c("Date","State","State25_Deaths", "State25_Confirmed")
colnames(df26) <- c("Date","State","State26_Deaths", "State26_Confirmed")
colnames(df27) <- c("Date","State","State27_Deaths", "State27_Confirmed")
colnames(df28) <- c("Date","State","State28_Deaths", "State28_Confirmed")
colnames(df29) <- c("Date","State","State29_Deaths", "State29_Confirmed")
colnames(df30) <- c("Date","State","State30_Deaths", "State30_Confirmed")
colnames(df31) <- c("Date","State","State31_Deaths", "State31_Confirmed")
colnames(df32) <- c("Date","State","State32_Deaths", "State32_Confirmed")
colnames(df33) <- c("Date","State","State33_Deaths", "State33_Confirmed")
colnames(df34) <- c("Date","State","State34_Deaths", "State34_Confirmed")
colnames(df35) <- c("Date","State","State35_Deaths", "State35_Confirmed")
colnames(df36) <- c("Date","State","State36_Deaths", "State36_Confirmed")
colnames(df37) <- c("Date","State","State37_Deaths", "State37_Confirmed")
colnames(df38) <- c("Date","State","State38_Deaths", "State38_Confirmed")
colnames(df39) <- c("Date","State","State39_Deaths", "State39_Confirmed")
colnames(df40) <- c("Date","State","State40_Deaths", "State40_Confirmed")
colnames(df41) <- c("Date","State","State41_Deaths", "State41_Confirmed")


df1 <- subset(df1,select = -c(2))
df2 <- subset(df2,select = -c(2))
df3 <- subset(df3,select = -c(2))
df4 <- subset(df4,select = -c(2))
df5 <- subset(df5,select = -c(2))
df6 <- subset(df6,select = -c(2))
df7 <- subset(df7,select = -c(2))
df8 <- subset(df8,select = -c(2))
df9 <- subset(df9,select = -c(2))
df10 <- subset(df10,select = -c(2))
df11 <- subset(df11,select = -c(2))
df12 <- subset(df12,select = -c(2))
df13 <- subset(df13,select = -c(2))
df14 <- subset(df14,select = -c(2))
df15 <- subset(df15,select = -c(2))
df16 <- subset(df16,select = -c(2))
df17 <- subset(df17,select = -c(2))
df18 <- subset(df18,select = -c(2))
df19 <- subset(df19,select = -c(2))
df20 <- subset(df20,select = -c(2))
df21 <- subset(df21,select = -c(2))
df22 <- subset(df22,select = -c(2))
df23 <- subset(df23,select = -c(2))
df24 <- subset(df24,select = -c(2))
df25 <- subset(df25,select = -c(2))
df26 <- subset(df26,select = -c(2))
df27 <- subset(df27,select = -c(2))
df28 <- subset(df28,select = -c(2))
df29 <- subset(df29,select = -c(2))
df30 <- subset(df30,select = -c(2))
df31 <- subset(df31,select = -c(2))
df32 <- subset(df32,select = -c(2))
df33 <- subset(df33,select = -c(2))
df34 <- subset(df34,select = -c(2))
df35 <- subset(df35,select = -c(2))
df36 <- subset(df36,select = -c(2))
df37 <- subset(df37,select = -c(2))
df38 <- subset(df38,select = -c(2))
df39 <- subset(df39,select = -c(2))
df40 <- subset(df40,select = -c(2))
df41 <- subset(df41,select = -c(2))



b  <- merge(x=df1,y=df2, by="Date",all=TRUE)
b1 <- merge(x=b,y=df3, by="Date",all=TRUE)
b2 <- merge(x=b1,y=df4, by="Date",all=TRUE)
b3 <- merge(x=b2,y=df5, by="Date",all=TRUE)
#b4 <- merge(x=b3,y=df6, by="Date",all=TRUE)
b5 <- merge(x=b3,y=df7, by="Date",all=TRUE)
b6 <- merge(x=b5,y=df8, by="Date",all=TRUE)
#b7 <- merge(x=b6,y=df9, by="Date",all=TRUE)
b8 <- merge(x=b6,y=df10, by="Date",all=TRUE)
#b9 <- merge(x=b8,y=df11, by="Date",all=TRUE)
b10 <- merge(x=b8,y=df12, by="Date",all=TRUE)
b11 <- merge(x=b10,y=df13, by="Date",all=TRUE)
b12 <- merge(x=b11,y=df14, by="Date",all=TRUE)
b13 <- merge(x=b12,y=df15, by="Date",all=TRUE)
b14 <- merge(x=b13,y=df16, by="Date",all=TRUE)
b15 <- merge(x=b14,y=df17, by="Date",all=TRUE)
b16 <- merge(x=b15,y=df18, by="Date",all=TRUE)
b17 <- merge(x=b16,y=df19, by="Date",all=TRUE)
b18 <- merge(x=b17,y=df20, by="Date",all=TRUE)
b19 <- merge(x=b18,y=df21, by="Date",all=TRUE)
b20 <- merge(x=b19,y=df22, by="Date",all=TRUE)
b21 <- merge(x=b20,y=df23, by="Date",all=TRUE)
b22 <- merge(x=b21,y=df24, by="Date",all=TRUE)
b23 <- merge(x=b22,y=df25, by="Date",all=TRUE)
b24 <- merge(x=b23,y=df26, by="Date",all=TRUE)
b25 <- merge(x=b24,y=df27, by="Date",all=TRUE)
b26 <- merge(x=b25,y=df28, by="Date",all=TRUE)
b27 <- merge(x=b26,y=df29, by="Date",all=TRUE)
b28 <- merge(x=b27,y=df30, by="Date",all=TRUE)
b29 <- merge(x=b28,y=df31, by="Date",all=TRUE)
b30 <- merge(x=b29,y=df32, by="Date",all=TRUE)
b31 <- merge(x=b30,y=df33, by="Date",all=TRUE)
b32 <- merge(x=b31,y=df34, by="Date",all=TRUE)
#b33 <- merge(x=b32,y=df35, by="Date",all=TRUE)
b34 <- merge(x=b32,y=df36, by="Date",all=TRUE)
b35 <- merge(x=b34,y=df37, by="Date",all=TRUE)
#b36 <- merge(x=b35,y=df38, by="Date",all=TRUE)
b37 <- merge(x=b35,y=df39, by="Date",all=TRUE)
b38 <- merge(x=b37,y=df40, by="Date",all=TRUE)
b39 <- merge(x=b38,y=df41, by="Date",all=TRUE)


ncol(b39)
nrow(b39)



#View(table(df$State))
#t <- nrow(df1) + nrow(df2) + nrow(df3)+ nrow(df4)+ nrow(df5)+ nrow(df6)+ nrow(df7)+ nrow(df8)+ nrow(df9)+ nrow(df10) +nrow(df11) + nrow(df12) + nrow(df13)+ nrow(df14)+ nrow(df15)+ nrow(df16)+ nrow(df17)+ nrow(df18)+ nrow(df19)+ nrow(df20) + nrow(df21) + nrow(df22) + nrow(df23)+ nrow(df24)+ nrow(df25)+ nrow(df26)+ nrow(df27)+ nrow(df28)+ nrow(df29)+ nrow(df30) + nrow(df31) + nrow(df32) + nrow(df33)+ nrow(df34)+ nrow(df35)+ nrow(df36)+ nrow(df37)+ nrow(df38)+ nrow(df39)+ nrow(df40) + nrow(df41)     

#write.csv(b39,"processed_daily_data.csv")


for(i in 1:ncol(b39))
{
  
  print("newcol")
  r <- 0
  for(j in 1:nrow(b39))
  {
    if(is.na(b39[j,i]) && r==0)
    {
      while(is.na(b39[j,i]))
      {
        b39[j,i] <- 0
        j <- j + 1
      }
      r <- 1
    }
    
    
  }
}


for(i in colnames(b39))
{
  q <- sum(is.na(b39$i))
  print(q)
}




b39[is.na(b39)] <- 0

plot_intro(b39)

b39$total_cases <-  as.numeric(apply(b39[,c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73)], 1, sum))
b39$total_deaths <-  as.numeric(apply(b39[,c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72)], 1, sum))

cases <- b39

rm(list=ls()[! ls() %in% c("cases","strains")])


#------------------------------------------------------------------------------------
# Done processing strains and cases data. Will combine them now.
#------------------------------------------------------------------------------------

write.csv(cases,"data_final/cleaned_data_cases.csv")
write.csv(strains,"data_final/cleaned_data_strains.csv")

df <- cases
df1 <- strains



rm(list=ls()[! ls() %in% c("df","df1")])

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

df1$Datee <- 2000-00-00
for(i in 1:length(rownames(df1)))
{
  df1$Datee[i] <-paste(paste(df1$year[i],substrRight(paste("0",df1$month[i], sep =""),2), sep ="-"),substrRight(paste("0",df1$day[i], sep =""),2), sep ="-")
}


t <- intersect(df$Date,df1$Datee)
length(t)

strains <- df1[df1$Datee %in% t,]


strains_freq <- as.data.frame(table(strains$Datee))
colnames(strains_freq) <- c("Date","Strains")


# Alpha strain
alpha1<-strains[strains$pangolin_lineage=="B.1.1.7",]
alpha_freq <- as.data.frame(table(alpha1$Datee))
colnames(alpha_freq) <- c("Date","Strains")

# Beta strain
beta1<-strains[strains$pangolin_lineage %in% c("B.1.351","B.1.351.2","B.1.351.3"),]
beta_freq <- as.data.frame(table(beta1$Datee))
colnames(beta_freq) <- c("Date","Strains")

# Gamma strain
gamma1<-strains[strains$pangolin_lineage %in% c("P.1","P.1.1","P.1.2"),]
gamma_freq <- as.data.frame(table(gamma1$Datee))
colnames(gamma_freq) <- c("Date","Strains")


# Delta strain
delta1<-strains[strains$pangolin_lineage %in% c("B.1.617.2","AY.1","AY.1","AY.3"),]
delta_freq <- as.data.frame(table(delta1$Datee))
colnames(delta_freq) <- c("Date","Strains")



cases <- df[df$Date %in% t,]
cases <- subset(cases,select = c(1,74,75))


df1 <- subset(df1,select = -c(12,13,14,15,16))


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

df$total_strains <- 0
for(i in 1:nrow(df))
{
  for(j in 1:nrow(strains_freq))
  {
    if(df$Date[i]==strains_freq$Date[j])
    {
      df$total_strains[i] <- strains_freq$Strains[j]
    }
  }
}

df$alpha_strains <- 0
for(i in 1:nrow(df))
{
  for(j in 1:nrow(alpha_freq))
  {
    if(df$Date[i]==alpha_freq$Date[j])
    {
      df$alpha_strains[i] <- alpha_freq$Strains[j]
    }
  }
}

df$beta_strains <- 0
for(i in 1:nrow(df))
{
  for(j in 1:nrow(beta_freq))
  {
    if(df$Date[i]==beta_freq$Date[j])
    {
      df$beta_strains[i] <- beta_freq$Strains[j]
    }
  }
}


df$gamma_strains <- 0
for(i in 1:nrow(df))
{
  for(j in 1:nrow(gamma_freq))
  {
    if(df$Date[i]==gamma_freq$Date[j])
    {
      df$gamma_strains[i] <- gamma_freq$Strains[j]
    }
  }
}

df$delta_strains <- 0
for(i in 1:nrow(df))
{
  for(j in 1:nrow(delta_freq))
  {
    if(df$Date[i]==delta_freq$Date[j])
    {
      df$delta_strains[i] <- delta_freq$Strains[j]
    }
  }
}


View(df)


write.csv(df,"data_final/combined_final_data.csv")



#------------------------------------------------------------------------------------
# Done with all data processing. Got the final data.
#------------------------------------------------------------------------------------

