library(class)
df<-read.csv("D:/R projects/dataset/wisc_bc_data.csv")

norm<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

str(df)
ncol(df)
df_n<-as.data.frame(lapply(df[,3:32], norm))
View(df_n)

#random
index<-sample(1:nrow(df_n),size=0.8*nrow(df_n))
train<-df_n[index,]
test<-df_n[-index,]
train_lab<-df[index,2]
test_lab<-df[-index,2]

k<-sqrt(nrow(train))

knnModel<-knn(train=train,test=test,cl=train_lab,k=21)
library(gmodels)
CrossTable(x=test_lab,y=knnModel,prop.chisq = FALSE)
#accuracy
mean(test_lab==knnModel)*100


#manual
nrow(df_n)
train_data<-df_n[1:469,]
test_data<-df_n[470:569,]
train_labM<-df[1:469,2]
test_labM<-df[470:569,2]


k<-sqrt(nrow(train_data))
knnModelM<-knn(train=train_data,test=test_data,cl=train_labM,k=21)
CrossTable(x=test_labM,y=knnModelM,prop.chisq = FALSE)
#accuracy
mean(test_labM==knnModelM)*100
