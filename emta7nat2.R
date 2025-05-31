#q1
library(gcookbook)
data("cabbage_exp")
View(cabbage_exp)

library(ggplot2)
ggplot(cabbage_exp,aes(x=Cultivar,y=Weight,fill = Date))+
  geom_col(position = "dodge",color="black")

#q2
data("uspopchange")
View(uspopchange)
library(tidyverse)
upc<-uspopchange %>% arrange(desc(Change)) %>% slice(1:25) 
upc

ggplot(upc,aes(x=Abb,y=Change,fill = Region))+geom_col()

ggplot(upc,aes(x=Abb,y=Change,fill = Region))+geom_col(color="black")+scale_fill_manual(values = c("red","blue","orange"))+
  labs(title = "Change vs Abb",x="State",y="Population Change")


#q3
age=c(25,30,22,28,21,30,28)
Employed=c(TRUE,FALSE,TRUE,TRUE,FALSE,TRUE,TRUE)
Gender=c("Male","Female","Female","Male","Female","Female","Male")

df<-data.frame(age,Employed,Gender)
df

df$Employed<-as.integer(df$Employed)
df$Gender<-as.integer(as.factor(df$Gender))
str(df)

df_mat<-as.matrix(df)
library(bigmemory)
big_df<-as.big.matrix(df_mat)

apply(big_df[], 2, sd)
apply(big_df[], 2, min)
apply(big_df[], 2, max)


#q4
name=c("Alice","Bob","Charlie")
age=c(25,30,22)
salary=c(50000,55000,48000)

dframe<-data.frame(name,age,salary)
dframe

df1<-dframe
df1$age_month<-(12*dframe$age)
df1
dframe

df2<-dframe
df2$salary<-df2$salary+(df2$salary*10)
df2

df3<-df2
df3$high_earner<-ifelse(df3$salary>55000,TRUE,FALSE)
df3

df3<-df3 %>% mutate(tax=salary*0.2,net_salary=salary-tax)
df3<-dframe %>% mutate(high_earner=salary>55000)
df3<-dframe %>% mutate(high_earner=ifelse(salary>55000,TRUE,FALSE))
df3

#q1
data("pressure")
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line(color="green")+geom_point(color="green")+
  geom_line(data=pressure*2)+geom_point(data=pressure*2)+
  geom_line(data=pressure/2,color="orange")+geom_point(data=pressure/2,color="orange")
  
  
#q2
data(mpg)
temp<-mpg %>% filter(cty>21) %>% select(cty,hwy) %>% ggplot(aes(cty,hwy))+geom_point()
print(temp)

#q3
num<-c(10,12,23,23,16,23,21,16)
my_sd<-function(x){
  
  len<-length(x)
  sum<-0
  
  for (i in 1:len) {
    sum<-sum+x[i]
    
  }
  
  avg<-sum/len
  
  
  sq_sum<-0
  for (i in 1:len) {
    sq_sum<-sq_sum+(x[i]-avg)^2
    
  }
  return(sqrt(sq_sum/(len-1)))
}

my_sd(num)
sd(num)

#q4
x<-c(16,18,14,22,27,17,17,22,20,22)

mymean<-function(x){
  return(sum(x)/length(x))
}
mymean(x)
mean(x)

#q5
data(mpg)
table(c(mpg$model,mpg$class))
mpg_1<-mpg %>% select(where(is.numeric))

nor<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
mpd_n<-lapply(list(mpg_1$displ,mpg_1$cty,mpg_1$hwy), nor)

#q2
library(ff)
library(ffbase)
setwd("D:/R projects")
shell("mkdir dir")
options(fftempdir = "D:/R projects/dir")

air_ff<-read.table.ffdf(file = "flights_sep_oct15 .txt",VERBOSE = TRUE,sep=",",header=TRUE,next.rows = 100000,colClasses=NA)

object.size(air_ff)
air<-read.table("flights_sep_oct15 .txt",header=TRUE,sep=",")
object.size(air)

#q3
df<-read.csv("D:/R projects/dataset/wisc_bc_data.csv")
df<-df[,-1]
table(df$diagnosis)
df$diagnosis<-factor(df$diagnosis,levels = c("M","B"),labels = c("MM","BB"))
table(df$diagnosis)

ncol(df)
df_n<-lapply(df[,2:31], nor)

#q4
curve(x^3-5*x+1,from = -5,to=5)

myfun<-function(x){
  return(1/(1+exp(-x+10)))
}

curve(myfun,from = -5,to=5,add = TRUE)
curve(1-myfun(x),from = -5,to=5,add = TRUE)
