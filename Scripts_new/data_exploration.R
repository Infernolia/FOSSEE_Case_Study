rm(list=ls())

#install.packages("ggplot2")
#install.packages("DataExplorer")
#install.packages("gridExtra")

library(ggplot2)
library(DataExplorer)
library(gridExtra)

df <- read.csv("data/final_data.csv")
View(df)

table(df$genbank_accession)

# Removing missing columns with ? entries
df <- subset(df,select = -c(1,2,6,20,25,26,27,28,30))
View(df)

plot_intro(df)

for(i in 1:length(colnames(df)))
{
  barplot(table(df[,c(i)]))
}

table(df$sex)