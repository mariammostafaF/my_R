###########################################################
#  section1
###########################################################
1+1
sqrt(25)
3^2
log(100)
exp(3)
8/0

x1=as.integer(5)
x1=5
x1<-5L
x2<-5
x3<-"a"
x4<-TRUE
x5<-2.02


typeof(x5)
class(x5)

is.numeric(x1)
is.character(x3)
is.logical(x4)


rm(x1)
rm(list=ls())


x<-4
x
(x<-4)
print(x)
paste("the value of x is",x)
sprintf("the value of x is %.2f",x)

x<-1:4                #int
x<-c(1,2,3,4)         #num
y<-6:9

z1<-x+y
z2<-x/y

z1<-1:5
x>3

x<-1:10
y<-x*x


help(plot)
x
y
plot(x,y,type="b",pch=19,col="red",xlab="X values",ylab="Y values",main="Basic Plot")

###########################################################
#  section 2
###########################################################
#vectors
x<-1:5
x
y<-c(1,"hi",TRUE)

#matrix 2d vector :: single datatype
z<-matrix(1:10,nrow=2,ncol = 5)
z
z<-matrix(c(1,2,"a","b"),nrow = 2,ncol=2,byrow = TRUE)
z



#vector to matrix
a<-1:5
b<-c("a","b","e",2,3)

a
b

z<-cbind(a,b)
z

z<-rbind(a,b)
z


#dataframe:multiple datatypes
df<-data.frame(
  st_id=1:3,
  st_name=c("ahmed","tareq","mariam"),
  gender=c("male","male","female")
)

View(df)

#access col
df$st_name
#add col
df$gpa <- c(3.2,4.0,2)
df[3,4]=2.4
View(df)
#row
df[2,]#row=2,all col
df

df[-2,]#remove col 2
df
df[1:2,-1]#rows 1,2 and all col except 1
df


#conditions
subset(df,gpa>=3)
subset(df,gender=="female")


#delete/select col
subset(df,select = c(st_name,gpa))
subset(df,select=-c(st_name))

#delete row
#name of student with gpa under 3
df$st_name[df$gpa<3]


#factoring categories or class
attach(df)
detach(df)

typeof(df$gender)
str(df$gender)
df$gender<-factor(df$gender,levels=c("male","female","other"))
str(df$gender)


#add row
df[4,]<-c(4,"haya","other",3.6)
df
str(df$gender)

df[4,]<- c(4,"haya","ee",3.6)    #error as ee not found
df


#pipeline %>%
library(tidyverse)
df %>% select(st_id,st_name)
df %>% filter(gpa<3 & gender=="female") %>% select(st_id,st_name)
typeof(df$gpa)
df$gpa<-as.numeric(df$gpa)
typeof(df$gpa)
df<-df %>% mutate(percentage = gpa*100/4.0)# add col
df


###########################################################
#  section 3
###########################################################
df<-read.csv("D:\\R projects\\dataset\\Airbnb_Open_Data.csv")
View(df)


#Missing values
colSums(is.na(df))   #Count missing values in each column

#Price
typeof(df$price)

df$price <- gsub("[\\$,]", "", df$price)  
df$service.fee <- gsub("[\\$,]", "", df$service.fee)   #remove $ , sign


df$price<-as.numeric(df$price)
df$service.fee<-as.numeric(df$service.fee)


colSums(is.na(df))
sum(is.na(df$price))
sum(is.na(df$service.fee))


df$service.fee[df$service.fee==""]<-NA
df$price[df$price==""]<-NA

#df$price <- ifelse(df$price == "", NA, df$price)
#df$service.fee <- ifelse(df$service.fee == "", NA, df$service.fee)


sum(is.na(df$service.fee))     #273
sum(is.na(df$price))           #247


#replace missing value with average
df$price[is.na(df$price)]<- round(mean(df$price,na.rm=TRUE))
df$service.fee[is.na(df$service.fee)]<- mean(df$service.fee,na.rm=TRUE)   #replace with mean or percentage


#service percentage
df$sevice.percent<-df$service.fee/df$price
mean(df$sevice.percent,na.rm=TRUE)             #0.24

df$service.fee[is.na(df$service.fee)]<-df$price*0.24


library(tidyr)
# %>% :pipeline
df<-df %>% replace_na(list(
  Construction.year=min(df$Construction.year,na.rm=TRUE),
  minimum.nights=1,
  number.of.reviews=0)
)


#majority t/f
table(df$instant_bookable)

#first way easy
df$instant_bookable[is.na(df$instant_bookable)]<-FALSE

#another way
df$instant_bookable[is.na(df$instant_bookable)]<- 
  ifelse(df$availability.365==0,FALSE,TRUE)

typeof(df$last.review)
df$last.review<-as.Date(df$last.review,'%m/%d/%y')

df$last.review[is.na(df$last.review)]<- 
  ifelse(df$number.of.reviews==0,NA,min(df$last.review,na.rm = TRUE))     #if number of reviews=0 replace last review with NA
#condition based on number of review

df$last.review[is.na(df$last.review)]<-min(df$last.review,na.rm=TRUE)     #replace NA with earliest review date without any condition

#Duplicates/Redundancy
#Data types
#Outliers

###########################################################
#  section 4
###########################################################
df<-read.csv("D:\\R projects\\dataset\\Airbnb_Cleaned_Data.csv")
View(df)

#datatypes
#missing values: mean,median,0

sum(is.na(df$price))
df$price[is.na(df$price)]<-0
sum(is.na(df$price))

library(tidyverse)
library(dplyr)

#1 sort

df<- df %>% arrange(desc(room.type))
sum(is.na(df$price))
nrow(df) #total no. of data set

#2 NA is replace with previous or next value

df$price<- ifelse(is.na(df$price),lag(df$price),df$price)  #lag replaces NA with the previous row's value
sum(is.na(df$price))
df$price[is.na(df$price)]<-mean(df$price,na.rm=TRUE)       #for the rest NA replace with mean
#View(df)
sum(is.na(df$price))

#3 mutate or edit

df<-df %>% mutate(price=
                    ifelse(is.na(price),mean(price,na.rm=TRUE),price))

#chatgpt
library(tidyr)
df <- df %>% mutate(price = replace_na(price, mean(price, na.rm = TRUE)))


#const year-Neighbor
sum(is.na(df$Construction.year))
df<-df %>% group_by(neighbourhood.group) %>%  
  mutate(Construction.year=ifelse(is.na(Construction.year),mean(Construction.year,na.rm = TRUE),Construction.year)) %>% 
  ungroup()

sum(is.na(df$Construction.year))
typeof(df$Construction.year)
df$Construction.year<-as.integer(df$Construction.year)


#Neighbor
res<-table(df$neighbourhood.group)
as.data.frame(res)
#difference in arrangement
table(df$neighbourhood.group)


#lowercase
df$neighbourhood.group<-sapply(df$neighbourhood.group,tolower)

#spelling
df$neighbourhood.group[df$neighbourhood.group=="brookln"]<-"brooklyn"
df$neighbourhood.group[df$neighbourhood.group=="manhatan"]<-"manhattan"
table(df$neighbourhood.group)                                          #majority
df$neighbourhood.group[df$neighbourhood.group==""]<-"manhattan"

res<-table(df$neighbourhood.group)
as.data.frame(res)






#price
fivenum(df$price)# min q1(25%)  q2 mean(50%)   q3(75%)   max


typeof(df$price)
df$price<-as.integer(df$price)

#IQR between q1 and q3 to get outlier
q1<-quantile(df$price,0.25)
q3<-quantile(df$price,0.75)
range<-IQR(df$price) #q3-q1
range
q3-q1
#outlier you can move before and after range by 1.5*IQR
upperlimit<-q3+1.5*range
lowerlimit<-q3-1.5*range

upperlimit
lowerlimit

df<-subset(df,df$price<upperlimit & df$price>lowerlimit)

view(df)


###########################################################
#  section 5
###########################################################
df<- read.csv("D:\\R projects\\dataset\\Titanic.csv")
nrow(df)


#classification -survived 1/0
#logistic regression knn
colSums(is.na(df)|df=="")
#Age
df$Age[is.na(df$Age)]<-mean(df$Age,na.rm=TRUE)

#Embarked
table(df$Embarked)
#majority
df$Embarked[df$Embarked==""]<-'S'
df<-
  subset(df,select=-c(PassengerId,Name,Ticket,Cabin))


df$Sex<-factor(df$Sex)
df$Embarked<-factor(df$Embarked)
df$Survived<-factor(df$Survived)

str(df$Sex)
str(df$Embarked)
str(df$Survived)


#training 80% test 20%
index<-sample(1:nrow(df),0.8*nrow(df))    #Randomly selects 80% of the row indices to be used for training
index

trainData<-df[index,]                     #Creates the training set using the sampled indices
testData<-df[-index,]

View(trainData)

#KNN

k<-round(sqrt(nrow(trainData)))           #A common heuristic for selecting k in KNN is √(training sample size)
k

#split features and output

y_train<-trainData$Survived
y_test<-testData$Survived


#scale features
library(dplyr)
trainData<-trainData %>% select_if(is.numeric) %>% scale()
testData<-testData %>% select_if(is.numeric) %>% scale()            #scale(): Standardizes features (mean=0, variance=1), 
#important for KNN because:
#KNN uses Euclidean distance, and unscaled features can skew results

#KNN
library(class)
predictions<-knn(train=trainData,test=testData,cl=y_train,k=27)
predictions


result<-data.frame(actual=y_test,pred=predictions)                  #actual vs predicted values
res<-table(actual=y_test,pred=predictions)


#accuracy
mean(y_test==predictions)

