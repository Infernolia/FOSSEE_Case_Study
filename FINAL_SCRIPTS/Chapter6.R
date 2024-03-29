rm(list=ls())
# Need the "stringr" package for "str_count" function.
#install.packages("stringr")
library(stringr)
# Need the "DataExplorer" package for "plot_intro" function.
#install.packages("DataExplorer")
library(DataExplorer)

#--------------------------------------------------------------------------------------

# Starting the processing of COVID-19 India cases
#--------------------------------------------------------------------------------------


df <- read.csv("data_final/cleaned_data_cases.csv")
df <- subset(df,select = -c(1))
df <- subset(df,select = c(1,2,6,7))
colnames(df) <- c("Date","State","Deaths","Confirmed")

df1 <- df[df$State=="Andhra Pradesh",]
df2 <- df[df$State=="Andaman and Nicobar Islands",]
df3 <- df[df$State=="Arunachal Pradesh",]
df4 <- df[df$State=="Assam",]
df5 <- df[df$State=="Bihar",]
df6 <- df[df$State=="Chandigarh",]
df7 <- df[df$State=="Chhattisgarh",]
df8 <- df[df$State=="Dadra and Nagar Haveli",]
df9 <- df[df$State=="Delhi",]
df10 <- df[df$State=="Goa",]
df11 <- df[df$State=="Gujarat",]
df12 <- df[df$State=="Haryana",]
df13 <- df[df$State=="Himachal Pradesh",]
df14 <- df[df$State=="Jammu and Kashmir",]
df15 <- df[df$State=="Jharkhand",]
df16 <- df[df$State=="Karnataka",]
df17 <- df[df$State=="Kerala",]
df18 <- df[df$State=="Lakshadweep",]
df19 <- df[df$State=="Ladakh",]
df20 <- df[df$State=="Madhya Pradesh",]
df21 <- df[df$State=="Maharashtra",]
df22 <- df[df$State=="Manipur",]
df23 <- df[df$State=="Meghalaya",]
df24 <- df[df$State=="Mizoram",]
df25 <- df[df$State=="Nagaland",]
df26 <- df[df$State=="Odisha",]
df27 <- df[df$State=="Puducherry",]
df28 <- df[df$State=="Punjab",]
df29 <- df[df$State=="Rajasthan",]
df30 <- df[df$State=="Sikkim",]
df31 <- df[df$State=="Tamil Nadu",]
df32 <- df[df$State=="Telengana",]
df33 <- df[df$State=="Tripura",]
df34 <- df[df$State=="Uttarakhand",]
df35 <- df[df$State=="Uttar Pradesh",]
df36 <- df[df$State=="West Bengal",]


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


b  <- merge(x=df1,y=df2, by="Date",all=TRUE)
b1 <- merge(x=b,y=df3, by="Date",all=TRUE)
b2 <- merge(x=b1,y=df4, by="Date",all=TRUE)
b3 <- merge(x=b2,y=df5, by="Date",all=TRUE)
b4 <- merge(x=b3,y=df6, by="Date",all=TRUE)
b5 <- merge(x=b4,y=df7, by="Date",all=TRUE)
b6 <- merge(x=b5,y=df8, by="Date",all=TRUE)
b7 <- merge(x=b6,y=df9, by="Date",all=TRUE)
b8 <- merge(x=b7,y=df10, by="Date",all=TRUE)
b9 <- merge(x=b8,y=df11, by="Date",all=TRUE)
b10 <- merge(x=b9,y=df12, by="Date",all=TRUE)
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
b33 <- merge(x=b32,y=df35, by="Date",all=TRUE)
b34 <- merge(x=b33,y=df36, by="Date",all=TRUE)


b34[is.na(b34)] <- 0


plot_intro(b34)

b34$total_cases <-  as.numeric(apply(b34[,c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73)], 1, sum))
b34$total_deaths <-  as.numeric(apply(b34[,c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72)], 1, sum))

cases <- b34

rm(list=ls()[! ls() %in% c("cases","strains")])




substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

cases$loa <- str_count(cases$Date, "-")
cases$loa
cases$year <- 0
cases$month <- 0
cases$day <- 0

a<- 0
b <-0
c <-0

for(i in 1:length(rownames(cases)))
{
  if(cases$loa[i]==2)
  {
    a = a + 1
    cases$year[i] <- substr(cases$Date[i],7,10)
    cases$month[i] <- substr(cases$Date[i],4,5)
    cases$day[i] <- substr(cases$Date[i],1,2)
  }
  else if(cases$loa[i]==1)
  {
    b = b+1
    cases$year[i] <- substr(cases$Date[i],1,4)
    cases$month[i] <- substr(cases$Date[i],6,7)
  }
  else
  {
    c = c+1
    cases$year[i] <- cases$Date[i]
  }
}
cases$datetemp <- 0

for(i in 1:length(rownames(cases)))
{
  if(cases$day[i]==0)
  {
    
    cases$datetemp[i] <-paste(paste(cases$year[i],substrRight(paste("0",cases$month[i], sep =""),2), sep =""),"00", sep ="")
    
  }
  else if(cases$month[i]==0)
  {
    cases$datetemp[i] <-paste(paste(cases$year[i],"00", sep =""),"00", sep ="")
  }
  else
  {
    cases$datetemp[i] <-paste(paste(cases$year[i],substrRight(paste("0",cases$month[i], sep =""),2), sep =""),substrRight(paste("0",cases$day[i], sep =""),2), sep ="")
  }
}



cases <- cases[order(cases$datetemp),]
cases <- subset(cases,select = -c(76,77,78,79))


write.csv(cases,"data_final/processed_data_cases.csv")

rm(list=ls())
#------------------------------------------------------------------------------------
# Done processing strains and cases data. Will combine them now.
#------------------------------------------------------------------------------------

df <- read.csv("data_final/processed_data_cases.csv")
df <- subset(df,select = -c(1))

df1 <- read.csv("data_final/cleaned_data_strains.csv")
df1 <- subset(df1,select = -c(1))


t <- intersect(df$datetemp,df1$date)
length(t)

cases <- df[df$datetemp %in% t,]
strains <- df1[df1$date %in% t,]


rm(list=ls()[! ls() %in% c("strains","cases")])

strains_freq <- as.data.frame(table(strains$date))
colnames(strains_freq) <- c("Date","Strains")


# Alpha strain
alpha1<-strains[strains$pangolin_lineage=="B.1.1.7",]
alpha_freq <- as.data.frame(table(alpha1$date))
colnames(alpha_freq) <- c("Date","Strains")

# Beta strain
beta1<-strains[strains$pangolin_lineage %in% c("B.1.351","B.1.351.2","B.1.351.3"),]
beta_freq <- as.data.frame(table(beta1$date))
colnames(beta_freq) <- c("Date","Strains")

# Gamma strain
gamma1<-strains[strains$pangolin_lineage %in% c("P.1","P.1.1","P.1.2"),]
gamma_freq <- as.data.frame(table(gamma1$date))
colnames(gamma_freq) <- c("Date","Strains")


# Delta strain
delta1<-strains[strains$pangolin_lineage %in% c("B.1.617.2","AY.1","AY.1","AY.3"),]
delta_freq <- as.data.frame(table(delta1$date))
colnames(delta_freq) <- c("Date","Strains")


cases[2:75] <- as.data.frame(sapply(cases[2:75], as.numeric))



cases$"Total_severity" <- as.double(0)
for(j in 1:nrow(cases))
{
  if(cases[j,"total_cases"]<=0 || cases[j,"total_deaths"]<0)
  {
    cases[j,"Total_severity"] <- 0.0
  }
  else
  {
    cases[j,"Total_severity"] <- as.double(cases[j,"total_deaths"])/as.double(cases[j,"total_cases"])
  }
  
}

cases$total_strains <- 0
for(i in 1:nrow(cases))
{
  for(j in 1:nrow(strains_freq))
  {
    if(cases$datetemp[i]==strains_freq$Date[j])
    {
      cases$total_strains[i] <- strains_freq$Strains[j]
    }
  }
}

cases$alpha_strains <- 0
for(i in 1:nrow(cases))
{
  for(j in 1:nrow(alpha_freq))
  {
    if(cases$datetemp[i]==alpha_freq$Date[j])
    {
      cases$alpha_strains[i] <- alpha_freq$Strains[j]
    }
  }
}

cases$beta_strains <- 0
for(i in 1:nrow(cases))
{
  for(j in 1:nrow(beta_freq))
  {
    if(cases$datetemp[i]==beta_freq$Date[j])
    {
      cases$beta_strains[i] <- beta_freq$Strains[j]
    }
  }
}


cases$gamma_strains <- 0
for(i in 1:nrow(cases))
{
  for(j in 1:nrow(gamma_freq))
  {
    if(cases$datetemp[i]==gamma_freq$Date[j])
    {
      cases$gamma_strains[i] <- gamma_freq$Strains[j]
    }
  }
}

cases$delta_strains <- 0
for(i in 1:nrow(cases))
{
  for(j in 1:nrow(delta_freq))
  {
    if(cases$datetemp[i]==delta_freq$Date[j])
    {
      cases$delta_strains[i] <- delta_freq$Strains[j]
    }
  }
}

cases <- subset(cases,select = -c(76))
View(cases)


write.csv(cases,"data_final/combined_final_data.csv")