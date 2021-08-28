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

df <- read.csv("data_final/cleaned_data_strains.csv")
colnames(df)




table(df$genbank_accession)

table(df$sex)

table(df$strain)[which(table(df$strain)>1)]

table(df$virus)

table(df$"date_submitted" )


table(df$segment)

table(df$gisaid_epi_isl)[which(table(df$gisaid_epi_isl)>1)]



for(i in 1:length(colnames(df)))
{
  barplot(table(df[,c(i)]))
}

plot_intro(df)
