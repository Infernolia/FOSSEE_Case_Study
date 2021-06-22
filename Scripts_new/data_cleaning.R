rm(list=ls())

df <- read.csv("data/final_data.csv")
View(df)

table(df$strain)[which(table(df$strain)>1)]

View(df[df[,"virus"]=="Raja Rao Mesipogu, Muttineni Radhakrishna, Nagamani K, Thrilok Chander B, Kalyani Putty, Ravikumar P, Sunitha P, Pankaj Singh D, Anand Kumar K, Amit A. Upadhyay, Steven Bosinger, Rama Amara",])


table(df$gisaid_epi_isl)[which(table(df$gisaid_epi_isl)>1)]



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


write.csv(df,"final_df1.csv")



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

View(table(df$"date_submitted" ))
View(df)

table(df$segment)
# Removing missing columns with ? entries
df <- subset(df,select = -c(1,2,4, 6,8,9,12,13, 15, 20,25,26,27,28,29,30))

#Columns removed are garbage index, index, virus(betacoronavirus for all), genbank_accession, region, country, region_exposure, country_exposure, segment, Nextstrain_clade, authors, url, title, paper_url, date submitted to GISAID, purpose_of_sequencing


View(df)

for(i in 1:length(colnames(df)))
{
  barplot(table(df[,c(i)]))
}



table(df$virus)

write.csv(df,"final_df1.csv")

