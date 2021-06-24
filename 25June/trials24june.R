rm(list=ls())
library(stringr)
library(ggplot2)

df <- read.csv("data.csv")
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




colnames(df1) <- c("Date","State",paste(word(df1[1,2],1),"_Deaths",sep=""), paste(word(df1[1,2],1),"_Confirmed",sep=""))
colnames(df2) <- c("Date","State",paste(word(df2[1,2],1),"_Deaths",sep=""), paste(word(df2[1,2],1),"_Confirmed",sep=""))
colnames(df3) <- c("Date","State",paste(word(df3[1,2],1),"_Deaths",sep=""), paste(word(df3[1,2],1),"_Confirmed",sep=""))
colnames(df4) <- c("Date","State",paste(word(df4[1,2],1),"_Deaths",sep=""), paste(word(df4[1,2],1),"_Confirmed",sep=""))
colnames(df5) <- c("Date","State",paste(word(df5[1,2],1),"_Deaths",sep=""), paste(word(df5[1,2],1),"_Confirmed",sep=""))
colnames(df6) <- c("Date","State",paste(word(df6[1,2],1),"_Deaths",sep=""), paste(word(df6[1,2],1),"_Confirmed",sep=""))
colnames(df7) <- c("Date","State",paste(word(df7[1,2],1),"_Deaths",sep=""), paste(word(df7[1,2],1),"_Confirmed",sep=""))
colnames(df8) <- c("Date","State",paste(word(df8[1,2],1),"_Deaths",sep=""), paste(word(df8[1,2],1),"_Confirmed",sep=""))
colnames(df9) <- c("Date","State",paste(word(df9[1,2],1),"_Deaths",sep=""), paste(word(df9[1,2],1),"_Confirmed",sep=""))
colnames(df10) <- c("Date","State",paste(word(df10[1,2],1),"_Deaths",sep=""), paste(word(df10[1,2],1),"_Confirmed",sep=""))
colnames(df11) <- c("Date","State",paste(word(df11[1,2],1),"_Deaths",sep=""), paste(word(df11[1,2],1),"_Confirmed",sep=""))
colnames(df12) <- c("Date","State",paste(word(df12[1,2],1),"_Deaths",sep=""), paste(word(df12[1,2],1),"_Confirmed",sep=""))
colnames(df13) <- c("Date","State",paste(word(df13[1,2],1),"_Deaths",sep=""), paste(word(df13[1,2],1),"_Confirmed",sep=""))
colnames(df14) <- c("Date","State",paste(word(df14[1,2],1),"_Deaths",sep=""), paste(word(df14[1,2],1),"_Confirmed",sep=""))
colnames(df15) <- c("Date","State",paste(word(df15[1,2],1),"_Deaths",sep=""), paste(word(df15[1,2],1),"_Confirmed",sep=""))
colnames(df16) <- c("Date","State",paste(word(df16[1,2],1),"_Deaths",sep=""), paste(word(df16[1,2],1),"_Confirmed",sep=""))
colnames(df17) <- c("Date","State",paste(word(df17[1,2],1),"_Deaths",sep=""), paste(word(df17[1,2],1),"_Confirmed",sep=""))
colnames(df18) <- c("Date","State",paste(word(df18[1,2],1),"_Deaths",sep=""), paste(word(df18[1,2],1),"_Confirmed",sep=""))
colnames(df19) <- c("Date","State",paste(word(df19[1,2],1),"_Deaths",sep=""), paste(word(df19[1,2],1),"_Confirmed",sep=""))
colnames(df20) <- c("Date","State",paste(word(df20[1,2],1),"_Deaths",sep=""), paste(word(df20[1,2],1),"_Confirmed",sep=""))
colnames(df21) <- c("Date","State",paste(word(df21[1,2],1),"_Deaths",sep=""), paste(word(df21[1,2],1),"_Confirmed",sep=""))
colnames(df22) <- c("Date","State",paste(word(df22[1,2],1),"_Deaths",sep=""), paste(word(df22[1,2],1),"_Confirmed",sep=""))
colnames(df23) <- c("Date","State",paste(word(df23[1,2],1),"_Deaths",sep=""), paste(word(df23[1,2],1),"_Confirmed",sep=""))
colnames(df24) <- c("Date","State",paste(word(df24[1,2],1),"_Deaths",sep=""), paste(word(df24[1,2],1),"_Confirmed",sep=""))
colnames(df25) <- c("Date","State",paste(word(df25[1,2],1),"_Deaths",sep=""), paste(word(df25[1,2],1),"_Confirmed",sep=""))
colnames(df26) <- c("Date","State",paste(word(df26[1,2],1),"_Deaths",sep=""), paste(word(df26[1,2],1),"_Confirmed",sep=""))
colnames(df27) <- c("Date","State",paste(word(df27[1,2],1),"_Deaths",sep=""), paste(word(df27[1,2],1),"_Confirmed",sep=""))
colnames(df28) <- c("Date","State",paste(word(df28[1,2],1),"_Deaths",sep=""), paste(word(df28[1,2],1),"_Confirmed",sep=""))
colnames(df29) <- c("Date","State",paste(word(df29[1,2],1),"_Deaths",sep=""), paste(word(df29[1,2],1),"_Confirmed",sep=""))
colnames(df30) <- c("Date","State",paste(word(df30[1,2],1),"_Deaths",sep=""), paste(word(df30[1,2],1),"_Confirmed",sep=""))
colnames(df31) <- c("Date","State",paste(word(df31[1,2],1),"_Deaths",sep=""), paste(word(df31[1,2],1),"_Confirmed",sep=""))
colnames(df32) <- c("Date","State",paste(word(df32[1,2],1),"_Deaths",sep=""), paste(word(df32[1,2],1),"_Confirmed",sep=""))
colnames(df33) <- c("Date","State",paste(word(df33[1,2],1),"_Deaths",sep=""), paste(word(df33[1,2],1),"_Confirmed",sep=""))
colnames(df34) <- c("Date","State",paste(word(df34[1,2],1),"_Deaths",sep=""), paste(word(df34[1,2],1),"_Confirmed",sep=""))
colnames(df35) <- c("Date","State",paste(word(df35[1,2],1),"_Deaths",sep=""), paste(word(df35[1,2],1),"_Confirmed",sep=""))
colnames(df36) <- c("Date","State",paste(word(df36[1,2],1),"_Deaths",sep=""), paste(word(df36[1,2],1),"_Confirmed",sep=""))
colnames(df37) <- c("Date","State",paste(word(df37[1,2],1),"_Deaths",sep=""), paste(word(df37[1,2],1),"_Confirmed",sep=""))
colnames(df38) <- c("Date","State",paste(word(df38[1,2],1),"_Deaths",sep=""), paste(word(df38[1,2],1),"_Confirmed",sep=""))
colnames(df39) <- c("Date","State",paste(word(df39[1,2],1),"_Deaths",sep=""), paste(word(df39[1,2],1),"_Confirmed",sep=""))
colnames(df40) <- c("Date","State",paste(word(df40[1,2],1),"_Deaths",sep=""), paste(word(df40[1,2],1),"_Confirmed",sep=""))
colnames(df41) <- c("Date","State",paste(word(df41[1,2],1),"_Deaths",sep=""), paste(word(df41[1,2],1),"_Confirmed",sep=""))



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
View(b39)


View(table(df$State))
#t <- nrow(df1) + nrow(df2) + nrow(df3)+ nrow(df4)+ nrow(df5)+ nrow(df6)+ nrow(df7)+ nrow(df8)+ nrow(df9)+ nrow(df10) +nrow(df11) + nrow(df12) + nrow(df13)+ nrow(df14)+ nrow(df15)+ nrow(df16)+ nrow(df17)+ nrow(df18)+ nrow(df19)+ nrow(df20) + nrow(df21) + nrow(df22) + nrow(df23)+ nrow(df24)+ nrow(df25)+ nrow(df26)+ nrow(df27)+ nrow(df28)+ nrow(df29)+ nrow(df30) + nrow(df31) + nrow(df32) + nrow(df33)+ nrow(df34)+ nrow(df35)+ nrow(df36)+ nrow(df37)+ nrow(df38)+ nrow(df39)+ nrow(df40) + nrow(df41)     

write.csv(b39,"processed_daily_data.csv")
