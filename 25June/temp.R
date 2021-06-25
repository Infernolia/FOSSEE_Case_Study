rm(list=ls())
#install.packages("TTR")
#install.packages("tseries")
library(tseries)
library("TTR")
library(stringr)
library(ggplot2)


df <- read.csv("final_df2_date_cleaned.csv")
df <- subset(df,select = -c(1))
View(df)