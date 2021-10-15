rm(list=ls())
# Need the "stringr" package for "str_count" function
#install.packages("stringr")
library(stringr)

#--------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------


# Reading the COVID-19 cases data set.
df <- read.csv("data_final/input_data_cases.csv",header=FALSE)

# There are 571 dates on which the cases were recorded in the data set. By iterating
# over all these dates the confirmed cases will be calculated by adding the corresponding
# number of cases of Indian and Foreign nationals infected with COVID-19.

for(i in 1:571)
{
  tcin <- 4*(i-1) + 2
  tcif <- 4*(i-1) + 3
  cured <- 4*(i-1) + 4
  death <- 4*(i-1) + 5
  
  date <- df[1,4*(i-1) + 2]
  colname <- paste0("conf",i)
  df$a <- 0
  df[1,colname] <- date
  df[2,colname] <- "Confirmed"
  for(j in 3:39)
  {
    df[j,colname] <- as.integer(df[j,tcin]) + as.integer(df[j,tcif])
  }
  
  names(df)[ncol(df)] <- colname
  
}

# Removing the redundant column 'a' created during the above step of preprocessing.
df = subset(df,select = -c(2286))

# Creating an empty data frame to store the final cases data. 
finaldf = data.frame()

# Appending the cases data in a common format of Date, State, Indian National cases,
# Foreign National cases, Cured, Deaths and Total cases in the 'finaldf' dataframe. 
for(i in 1:571)
{
  tcin <- 4*(i-1) + 2
  tcif <- 4*(i-1) + 3
  cured <- 4*(i-1) + 4
  death <- 4*(i-1) + 5
  
  date <- df[1,4*(i-1) + 2]
  colname <- paste0("conf",i)
  tempdf = subset(df,select = c(1,tcin,tcif,cured,death))
  tempdf$Confirmed <- 0
  for(j in 3:39)
  {
    tempdf[j,"Confirmed"] <- as.integer(df[j,colname])
  }
  
  tempdf$Date <- date
  tempdf <- tempdf[-c(1,2,39), ]
  colnames(tempdf)<- c("State.UnionTerritory","ConfirmedIndianNational","ConfirmedForeignNational","Cured","Deaths","Confirmed","Date")
  tempdf  <- tempdf[, c(7,1,2,3,4,5,6)]
  if(i==1)
  {
    finaldf <- tempdf
  }
  else
  {
    finaldf <- rbind.data.frame(finaldf,tempdf)
  }
  
}

# Saving the final data frame to a csv file.
write.csv(finaldf,"data_final/cleaned_data_cases.csv")

#-----------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------


rm(list=ls())

df <- read.csv("data_final/input_data_strains.csv")


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

write.csv(strains,"data_final/cleaned_data_strains.csv")