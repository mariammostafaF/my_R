install.packages("factoextra",include_dependencies=TRUE) #used for visualizing clustering and PCA
library(tidyverse)
library(factoextra)
library(dplyr)


df<-read.csv("D:\\R Projects\\dataset\\CustomerCreditData.csv")                #supervised              unsupervised

colSums(is.na(df))                                                                                                          #labeled(x-y)            unlabeled(x-features)

df$MINIMUM_PAYMENTS[is.na(df$MINIMUM_PAYMENTS)]<-ifelse(df$PAYMENTS==0,0,df$PAYMENTS*0.25) 
colSums(is.na(df))
df<-na.omit(df)       # delete missing                                                                                      #log reg,liner reg,knn   k means,pca

colSums(is.na(df))

ggplot(df)+
  aes(x=BALANCE,y=PAYMENTS)+geom_point()  #outliers
ggplot(df,aes(x=BALANCE,y=PAYMENTS))+geom_point()
nrow(df)
view(df)

#IQR
for(col in c("BALANCE","PAYMENTS","PURCHASES")){
  q1<-quantile(df[[col]],0.25)
  q3<-quantile(df[[col]],0.75)
  iqr<-q3-q1
  upperlimit<-q3+2.5*iqr
  lowerlimit<-q1-2.5*iqr
  df<-df %>% filter(df[[col]] >= lowerlimit & df[[col]] <= upperlimit)
}
nrow(df)
iqr
q1
q3

ggplot(df)+
  aes(x=BALANCE,y=PAYMENTS)+geom_point()

#before scaling make sure all col are integer
str(df)
df<-subset(df,select = -c(CUST_ID))
#normalize
cust_scaled<-scale(df)
view(cust_scaled)                             

#kmeans -> wss -> within cluster square sum(smaller better ->less distance)
#k better at 3 or 4
fviz_nbclust(cust_scaled,kmeans,method = "wss")   #show us curve to choose best k elbow method

kmeans_model<-kmeans(cust_scaled,centers =3,nstart = 25)  # apply kmeans tries 25 different random starting points and chooses the best
kmeans_model$size                                         #clusters the customers into 3 segments      

df$CLASS<-kmeans_model$cluster      #label dataset
view(df)
fviz_cluster(kmeans_model, data = cust_scaled)
help("fviz_cluster")

ggplot(df)+
  aes(x=BALANCE,y=PAYMENTS,fill=CLASS)+geom_point()
ggplot(df)+
  aes(x=BALANCE,y=PAYMENTS,color = factor(CLASS))+geom_point()

#pca dimensionalty reduction
#to reduce features
pca<-prcomp(cust_scaled,mean=TRUE,scale. = TRUE)
pca_data<-as.data.frame(pca$x[ ,1:10])             #x=feature
view(pca_data)
summary(pca)             #90-95% bec 100->overfit


#kmeans with pca data
kmeans_model<-kmeans(pca_data,centers =3,nstart = 25)
df$CLASS<-kmeans_model$cluster
ggplot(df)+
  aes(x=BALANCE,y=PAYMENTS,color = factor(CLASS))+geom_point()
view(df)
fviz_cluster(kmeans_model, data = cust_scaled)
