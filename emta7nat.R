#question 1
data(pressure)
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()+
  geom_line(data = pressure/2,col="red")+geom_point(data = pressure/2,col="red")+
  geom_line(data = pressure*2,col="green")+geom_point(data = pressure*2,col="green")
#question 2
library(gcookbook)
data("uspopchange")
View(uspopchange)

upc<-uspopchange %>% arrange(desc(Change)) %>% slice(1:10)
upc
ggplot(upc,aes(x=Abb,y=Change,fill = Region))+scale_fill_manual(values = c("red","green","blue","yellow"))+labs(x="state",y="Change")+geom_col(col="black")



#question 3
library(gcookbook)
data("cabbage_exp")
ggplot(cabbage_exp,aes(x=Cultivar,y=Weight,fill = Date))+geom_col(position = "dodge",col="black")

#question 4
library(ggplot2)
data("diamonds")
dim(diamonds)

str(diamonds)
library(dplyr)
diamonds_n<- diamonds %>% select(where(is.numeric))
diamonds_n <- diamonds[sapply(diamonds, is.numeric)]
View(diamonds_n)
dim(diamonds_n)

colnames(diamonds_n)
diamond_mean<-as.data.frame(lapply(diamonds_n[,c("carat","depth","table","price","x","y","z")],mean))
diamond_mean

diamonds_fact<-diamonds %>% select(!where(is.numeric))
diamonds_fact <- diamonds[sapply(diamonds, Negate(is.numeric))]
View(diamonds_fact)

col_table<-table(diamonds_fact$color)
barplot(col_table,main = "Distribution of Diamond Colors", col = "skyblue", ylab = "Count",xlab = "Color")

diamonds_char <- mapply(table, diamonds_fact, SIMPLIFY = FALSE) 
diamond_char<-lapply(diamonds_fact[,c("cut","color","clarity")], table)
diamond_char
diamonds_fact %>% count(cut)
diamonds_fact %>% count(color)
diamonds_fact %>% count(clarity)

#question 3
setwd("D:/R Projects")
need0<-read.csv("need_puf_2014.csv",header = TRUE,sep = ",")
view(need0)
object.size(need0)
str(need0)


getclass<-function(x){
  return(class(need0[,x]))
}
classes<-unlist(lapply(colnames(need0),getclass))
classes


getchar<-which(classes=="character")
getchar

for(i in getchar){
  need0[,i]<-factor(need0[,i])
}

classes<-unlist(lapply(colnames(need0),getclass))
classes

getfact<-which(classes=="factor")
getfact

for(i in getfact){
  need0[,i]<-as.integer(need0[,i])
}
classes<-unlist(lapply(colnames(need0),getclass))
classes

getint<-which(classes=="integer")
getint


write.table(need0,"need_integer.csv",sep = ",",col.names = TRUE,row.names = FALSE)
library(bigmemory)
need_mat<-read.big.matrix("need_integer.csv",sep = ",",header = TRUE,type = "double",backingfile = "need.bin",descriptorfile = "need.desc")
object.size(need_mat)


library(biganalytics)
library(bigtabulate)
view(need_mat)
colnames(need_mat)
bands<-bigsplit(need_mat,ccols = "EE_BAND",splitcol = "PROP_AGE")
bands


wbcd<-read.csv("wisc_bc_data.csv")
wbcd<-wbcd[,-1]

wbcd$diagnosis<-factor(wbcd$diagnosis,levels = c("B","M"),labels = c("BBBB","MMMM"))
str(wbcd)
table(wbcd$diagnosis)

normfun<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
str(wbcd)
dim(wbcd)
wbcd_n<-as.data.frame(lapply(wbcd[,2:31], normfun))

nrow(wbcd_n)
wbcd_train<-wbcd_n[1:469,]
wbcd_test<-wbcd_n[470:569,]
wbcd_train_labels<-wbcd[1:469,1]
wbcd_test_labels<-wbcd[470:569,1]

index<-sample(1:nrow(wbcd_n),size = 0.7*nrow(wbcd_n))
train<-wbcd_n[index,]
test<-wbcd_n[-index,]


library(class)
k<-sqrt(nrow(wbcd_train))

wbcd_predictons<-knn(train = wbcd_train,test = wbcd_test,cl=wbcd_train_labels,k=21)
wbcd_predictons


curve(x^3+5*x+1,from = -20,to=20,col="red")
help(curve)
fun<-function(xvar){
  return(1/(1+exp(-xvar+10)))
}
curve(fun,from = 0,to=20,add = TRUE)
curve(1-fun(x),from = 0,to=20,add = TRUE)

help("scale_fill_manual")
example(scale_fill_manual)


#question
install.packages("ff")
install.packages("ffbase")

dir.create("air_ffdf")
library(ff)
library(ffbase)

airline.ff <- read.table.ffdf(file = "flights_sep_oct15.txt", 
                              sep = ",", 
                              header = TRUE,
                              VERBOSE = TRUE,
                              next.rows = 10000, 
                              colClasses = NA)
airline_1 <- read.table("flights_sep_oct15.txt", 
                        sep = ",", 
                        header = TRUE)
# Size of ffdf object
print("Size of airline.ff:")
format(object.size(airline.ff), units = "auto")

# Size of regular data frame
print("Size of airline_1:")
format(object.size(airline_1), units = "auto")

#std 
std<-function(x){
  n<-length(x)
  mean_x<-mean(x)
  sum<-0
  
  for (i in 1:n) {
    sum<-sum+(x[i]-mean_x)^2
  }
  s<-sqrt(sum/(n-1))
  return(s)
}
data <- c(10, 12, 23, 23, 16, 23, 21, 16)
std(data)


#q
# === Step 1: Import RData file ===
load("dataset.RData")  # Ensure "dataset.RData" exists in the working directory

# === Step 2: Extract Age vector ===
# Adjust this if the Age vector is named differently
datasetAge <- dataset$Age  

# === Step 3: Export Age vector to text and CSV files ===
write.table(datasetAge, file = "age1.txt", sep = ",", row.names = FALSE, col.names = "Age", quote = FALSE)
write.csv(datasetAge, file = "age2.csv", row.names = FALSE, quote = FALSE)

# === Step 4: Export entire dataset to text and CSV files ===
write.table(dataset, file = "dataset1.txt", sep = ",", row.names = FALSE, quote = FALSE)
write.csv(dataset, file = "dataset2.csv", row.names = FALSE, quote = FALSE)

