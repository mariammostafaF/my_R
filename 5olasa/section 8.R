df<-read.csv("D:\\R Projects\\dataset\\Titanic.csv")
View(df)

colSums(is.na(df))

df$Age<-ifelse(is.na(df$Age),mean(df$Age,na.rm=TRUE),df$Age)
df$Age[is.na(df$Age)]<-mean(df$Age,na.rm=TRUE)

colSums(is.na(df))

#categorical: barchart
library(ggplot2)
str(df)
df$Survived<-factor(df$Survived,levels = c(0,1),labels = c("No","Yes"))
str(df)

ggplot(df,aes(x=Survived))+geom_bar(width = 0.5,color="black",fill="red")+
  ylab("number of surrvived")+xlab("yes/no")+ggtitle("survival rates")
help("theme")
ggplot(df)+
  aes(x=Survived)+
  geom_bar(width=0.5,fill="red",col="green")    #count on y

ggplot(df,aes(x=interaction(Survived,Pclass),fill = Sex))+
  geom_bar(width = 0.5,position = "dodge")+labs(title ="sur rate",x="sur",y="no of pass" ) 

ggplot(df,aes(x=Survived,fill = Sex))+
  geom_bar(width = 0.5)+labs(title ="sur rate",x="sur",y="no of pass" )+facet_wrap(.~Pclass)

ggplot(df)+
  aes(x=Survived,fill=Sex)+
  geom_bar(width=0.5)+
  facet_wrap(.~Pclass)+
  labs(title = "passenger survival",x="survival",y="number of passengers")                       # ~ concat
                                                                                                 #split into separate panels for each passenger class (Pclass)
View(df)
help("cut")
df$AgeClass<-cut(df$Age,breaks = c(0,21,60,Inf),  #cut() divides continuous Age into categories
            labels = c("Teen","Adult","Elder"))
str(df)
ggplot(df)+
  aes(x=AgeClass,fill=Survived)+geom_bar()+theme(axis.title = element_text(color = "purple"))
ggplot(df,aes(x=AgeClass,fill = Survived))+geom_bar()+theme(axis.title = element_text(color = "purple"))
help(geom_bar)
ggplot(df, aes(x=AgeClass, fill=Sex)) + geom_bar(position="fill") + facet_wrap(.~Survived)
ggplot(df, aes(x=AgeClass, fill=Sex)) + geom_bar(position="stack") + facet_wrap(.~Survived)

#numerical: histogram,density plot,boxplot..
ggplot(df)+
  aes(x=Fare)+
  geom_histogram(bins=10)           #outliers

ggplot(df)+
  aes(x=Fare)+
  geom_boxplot()

boxplot(df$Fare)
#IQR
q1<-quantile(df$Fare,0.25)
q2<-quantile(df$Fare,0.75)
IQR<-q2-q1
lowerlimit<-q1-1.5*IQR
upperlimit<-q2+1.5*IQR    #as you decrease 1.5 no outliers will appear
upperlimit
lowerlimit
library(tidyverse)
df<-df %>% filter(Fare <= upperlimit & Fare>=lowerlimit)
df<-df %>% filter(Fare <= upperlimit & Fare>0)
boxplot(df$Fare)
ggplot(df)+
  aes(x=Fare)+
  geom_boxplot()

ggplot(df)+
  aes(x=Fare)+
  geom_histogram(bins=10)


ggplot(df)+
  aes(x=Fare)+
  geom_density()


ggplot(df)+
  aes(x=Fare)+
  geom_boxplot()

ggplot(df)+
  aes(x=Survived,y=Fare,fill = Sex)+
  geom_boxplot()

str(df$Pclass)
ggplot(df)+
  aes(x=Age,y=Fare,fill=Pclass)+
  geom_point()
ggplot(df)+
  aes(x=Age,y=Fare,color=factor(Pclass))+
  geom_point()

