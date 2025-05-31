#week 8
################################################################################
#                                 PART1
################################################################################
#bar graphs
install.packages("ggplot2")
library(ggplot2)
install.packages("gcookbook")
library(gcookbook)

data("uspopchange")
View(uspopchange)

install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
upc<-uspopchange %>% arrange(desc(Change)) %>% slice(1:10)
upc
upc<-uspopchange %>% arrange(desc(Change)) %>% head(10)
View(upc)

ggplot(upc,aes(x=Abb,y=Change,fill=Region))+geom_col()

ggplot(upc,aes(x=Abb,y=Change,fill=Region))+
  scale_fill_manual(values = c("brown","pink"))+geom_col()

ggplot(upc,aes(x=Abb,y=Change,fill=Region))+
  scale_fill_manual(values = c("lightblue","pink"))+
  geom_col(color="grey")+xlab("The State")+ylab("The  Change")


################################################################################
#                                 PART2
################################################################################
library(ggplot2)
library(gcookbook)
data("climate")
view(climate)


climate_sub<-climate %>% 
  filter(Source=="Berkeley" & Year>=1900) %>% 
  mutate(pos=Anomaly10y>=0)
view(climate_sub)

ggplot(climate_sub,aes(x=Year,y=Anomaly10y,fill=pos))+
  scale_fill_manual(values = c("green","blue"))+
  geom_col(color="black")


#week 10
##############################################################
#          PART1
##############################################################
curve(x^3-5*x,from=-4,to=4,col="red")
curve(x^2+4,add=TRUE,from=-2,to=2,col="green")

##############################################################
#          PART2
##############################################################
myfun<-function(xvar){
  1/(1+exp(-xvar+10))
}

curve(myfun,from=0,to=20,col="red")

curve(1-myfun(x),add=TRUE,col="blue")

curve((1-myfun(x))/2,add=TRUE,col="green")

##############################################################
#          PART3
##############################################################
#making a basic bar graph
install.packages("ggplot2")
install.packages("gcookbook")
library(ggplot2)
library(gcookbook)


data(pg_mean)
View(pg_mean)
ggplot(pg_mean,aes(x=group,y=weight))+geom_col(width = 0.5)

data("cabbage_exp")
View(cabbage_exp)

ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_col(position = "dodge")

ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_col(position = "dodge",colour="black")

ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_col(position = "dodge",colour="grey")+
  scale_fill_brewer(palette = "Pastel1")

ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_col(position = "dodge",colour="grey")+
  scale_fill_manual(values = c("pink","royalblue"))

##############################################################
#          PART4
##############################################################
library(ggplot2)
data("diamonds")
View(diamonds)

table(diamonds$cut)
ggplot(diamonds,aes(x=cut))+geom_bar()

table(diamonds$carat)
ggplot(diamonds,aes(x=carat))+geom_bar()

#week11
##############################################################
setwd("D:\\R projects")
getwd()

x<-c(T,F,T)
x<- as.integer(x)
x

df<-read.csv("need_puf_2014.csv",header = TRUE,sep = ",")
View(df)

colnames(df)
str(df)

fun<-function(x){
  class(df[,x])                  #data type
}
yy<-lapply(colnames(df),fun)     #apply fun to all col in df
yy

classes<-unlist(lapply(colnames(df),function(x){    #removed listing
  class(df[,x])
}))
classes


#Converting Character Columns to Factors
newfun<-which(classes=="character")
newfun

for (i in newfun) {
  df[ ,i]<-factor(df[ ,i])
}

classes<-unlist(lapply(colnames(df),function(x) {
  class(df[,x])
}))
classes


#Converting Factors to Integers
newfun<-which(classes=="factor")
newfun

for (i in newfun) {
  df[ ,i]<-as.integer(df[ ,i])
}

classes<-unlist(lapply(colnames(df),function(x){
  class(df[,x])
}))
classes

View(df)

newfun<-which(classes=="integer")
newfun


#after converting all columns to integer
#write clean data to hard disk
write.table(df,"need_data.csv",sep = ",",row.names = FALSE,col.names = TRUE)


#You install and use bigmemory to handle data efficiently in memory:
#This creates a memory-efficient object backed by binary files.
install.packages("bigmemory")
library(bigmemory)

need.mat<-read.big.matrix("need_data.csv",header = TRUE,sep = ",",type = "double",
                          backingfile = "need_data.bin",
                          descriptorfile = "need_data.desc")
need.mat     #object
object.size(need.mat)
dim(need.mat)
dimnames(need.mat)
head(need.mat)

install.packages("bigtabulate")
library(bigtabulate)
library(biganalytics)


bigtable(need.mat,c("PROP_AGE"))         #like table()
bigtable(need.mat,c("PROP_AGE","PROP_TYPE"))
summary(need.mat[ ,"Econs2012"])


#Splitting by Group
need.bands<-bigsplit(need.mat,ccols = "EE_BAND",splitcol = "Econs2012")
need.bands


colmean(need.mat)
apply(need.mat, 2, sd)
