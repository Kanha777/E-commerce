# Data manipulation
library("dplyr")
library("knitr")
library("tidyr") 

# Cluster
library("kohonen")
library("NbClust")
library("factoextra")

library(caret)

#set directory##
setwd("C:/Users/User/Desktop/data science project/ecommerce/loyal customer")

#read file##
df=read.table("rfm.txt",header=FALSE)

#naming file#

df=as.data.frame(cbind(df[,1],df[,2],df[,4]))
name=c("id","date","amount")
names(df)=name

#converting date format#
df[,2]=as.Date(as.character(df[,2]),"%Y%m%d")

str(df)
summary(df)


#missing value treatment#
df <- df %>% mutate(amount = replace(amount, amount<=0, NA))

sum(is.na(df))

df=na.omit(df)

df


##calculate rfm##

RFM <- df %>% group_by(id) %>% 
  summarise(recency=as.numeric(Sys.Date()-max(as.Date(date))),
            frequency=n(), monetary = sum(amount))



##eda#
library(DataExplorer)
create_report(RFM)

#lowest recency and highest frequency and monetary value our best customer #

boxplot(RFM$recency,title="recency")
summary(RFM$recency)
#1st quartile8250#

boxplot(RFM$frequency)
summary(RFM$frequency)

#above50#

boxplot(RFM$monetary)
summary(RFM$monetary)

#above 2000##

loyal=subset(RFM,RFM$recency<=8250 & RFM$recency>=50 & RFM$monetary>=4000)
loyal=save.as("loyal.csv")

#customer segmentation#
k2m <- kmeans(RFM, centers = 6, iter.max = 500, nstart = 10) 
k2m

fviz_cluster(k2m, data = RFM)
