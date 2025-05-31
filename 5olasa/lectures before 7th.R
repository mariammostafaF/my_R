##################################################
#lec1
##################################################
1+2
5 * 10
abs(-10)
?abs
help(abs)
example(abs)
example(ggplot)
xx <- -9:9
plot(xx, sqrt(abs(xx)),  col = "blue")

#Data types: numeric, character, and logical
mynumeric <- 0.2
mynumeric <- 10
mylogical <- F
mylogical <- T
mylogical <- f
mycharacter <-"Ahmed Fahmy car"
mycharacter2 <-'Ahmed Fahmy car'
mycharacter <-"Ahmed Fahmy's car"
mycharacter2 <-'Ahmed Fahmy"s car'
x = c(3, 5, 7, 9)
myfactor<- c("male", "female", "female")
myfactor2<- as.factor(c("male", "female", "female"))


vec1<- c(1,2,2,23,0.2)
vec2<- c(T, F, FALSE, TRUE, F)
vec3 <-c("a", "b", "b", "my little story", "z","20")
vec4<- c("male", "female", "female", "male", "female")
vec4<- as.factor(c("male", "female", "female", "male", "female", "Female"))

#If you put different data types in one vector,
#It will transform them following the following rule:
#
#TRUE/FALSE + Numeric ---> Numeric where TRUE=1 and FALSE=0
#Numeric + Character --> character
vec5 <- c(1, TRUE, "b")
vec5
vec6<- c(1, TRUE)
vec6

#The LISTS
mylist <- list(vec1, vec2,vec3, 20, list(vec1, vec2, vec3), mean)
mylist


##################################################
#lec2
##################################################
xx= -9:9
xx
1:10
seq(1, 9, by=3)
seq(1, 9, by=pi)
seq(1.575, 5.125, by=0.05)
print(pi)
pi
print(matrix(c(1, 2, 3, 4, 5,7), 2,3))
matrix(c(1, 2, 3, 4, 5,7), 2,3)

print("The zero occurs at", 2*pi, "radians", "\n")
print("The zero occurs at")
print(2*pi)
print("radians")

cat("The zero occurs at", 2*pi, "radians", "\n")

fib <- c(0, 1, 1, 2,3,5,8,13,21,34)
y<- fib + 2
z=y - 5

log(100)
log10(100)

4^2
4**2

x <- 25
y=50
z <- x*y
z

data()
data(trees)
head(trees)
nrow(trees)
ncol(trees)
dim(trees)
str(trees)
mean(trees$Girth)
sd(trees$Height)
max(trees$Girth)
min(trees$Girth)
fivenum(trees$Girth)#min,first quad,median,third quad,max
summary(trees)

attach(trees)
mean(Girth)
sd(Height)
max(Girth)
min(Girth)

detach(trees)


die <- 1:6
sample(die, 1)
sample(die, 2)

sample(die, 2)##without replacement
sample(die, 2, replace=TRUE)##with replacement


dice <- sample(die, 2, replace=TRUE)
dice

help(sample)
roll <- function(){
  die <- 1:6
  dice<-sample(die, 2, replace=TRUE) #Rolls two dice
  dice
}
roll
roll()
roll()
roll()
roll()
roll()
roll()
roll()

##################################################
#lec3
##################################################
ages <- c(5,6)
names <- c("ahmed", "marwan")


friends <- data.frame(names, ages)
friends
str(friends)
friends$names
friends$ages


sum(friends$ages)
friends
friends[1, ]
friends[1, 1]
friends[ ,1]

install.packages("tidyverse")
library(tidyverse)
starwars
view(starwars)
x<-starwars %>% 
  filter(height>150 & mass<200) %>% 
  mutate(height_in_meters=height/100) %>% 
  select(height_in_meters, mass) %>% 
  arrange(-mass)
x
plot(x$height_in_meters, x$mass)
ggplot(x,aes(x=height_in_meters,y=mass))+geom_line()+geom_point()
str(starwars$sex)
starwars<-starwars %>% filter(!(sex=="none"|is.na(sex)))
                      
str(starwars$sex)
starwars$sex<-factor(starwars$sex)
ggplot(starwars,aes(x=height,y=mass,fill = sex))+geom_point()

x2<-starwars %>% 
  filter(height>150 & mass<200) %>% 
  mutate(height_in_meters=height/100) %>% 
  select(height_in_meters, mass) %>% 
  arrange(-mass)%>% 
  plot()

##################################################
#lec4
##################################################
getwd()
setwd("D:/R Projects")
getwd()
wbcd = read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
View(wbcd)
str(wbcd)


wbcd<-wbcd[,-1]    #remove first col
head(wbcd)

table(c(wbcd$diagnosis))
wbcd$pos<-ifelse(wbcd$diagnosis=="B",TRUE,FALSE)

table(c(wbcd$diagnosis,wbcd$pos))
wbcd$diagnosis<-factor(wbcd$diagnosis,levels = c("B","M"),labels = c("Benign","Malignant"))    #replacing

table(wbcd$diagnosis)
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])
summary(wbcd[c(2,5)])


#function
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))

wbcd_n<-lapply(wbcd[2:31], normalize)
wbcd_n

wbcd_n<-as.data.frame(lapply(wbcd[2:31], normalize))
wbcd_n
##################################################
#lec5
##################################################
##NORMALIZATION
normalize <- function(x){
  return((x-min(x))/(max(x)- min(x)))
}
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

dim(wbcd)
wbcd_n<-lapply(wbcd[ ,2:31], normalize)
wbcd_n <- as.data.frame(wbcd_n)
wbcd_n <- as.data.frame(lapply(wbcd[, 2:31], normalize))


wbcd_n<-as.data.frame(lapply(wbcd[,2:31], normalize))#Normalizes all numeric predictor columns. Excludes diagnosis (column 1).

nrow(wbcd_n)
wbcd.train<-wbcd_n[1:469, ]
wbcd.test<-wbcd_n[470:569, ]
wbcd.train.labels<-wbcd[1:469,1 ]
wbcd.test.labels<-wbcd[470:569,1 ]


n_rows <- nrow(wbcd_n)
?sample
indices <- sample(1:nrow(wbcd_n), size = 0.7 * nrow(wbcd_n))

train <- wbcd_n[indices, ]
test <- wbcd_n[-indices, ]

install.packages("class")
library(class)

k<-round(sqrt(nrow(wbcd.train)))           #A common heuristic for selecting k in KNN is √(training sample size)
k
k<-sqrt(nrow(wbcd.train))


wbcd_test_prediction<-knn(train = wbcd.train,test = wbcd.test,
                          cl=wbcd.train.labels,k=21)
install.packages("gmodels")
library(gmodels)

CrossTable(x=wbcd.test.labels,y=wbcd_test_prediction,prop.chisq = FALSE)

##################################################
#lec6
##################################################
#creating a scatter plot
setwd("D:/R projects")
data("mtcars")
View(mtcars)
mtcars

plot(mtcars$wt, mtcars$mpg,type = "p",main="Test Plot")
plot(mtcars$wt, mtcars$mpg, type = "p", col = "darkgreen", pch = 19,
     xlab = "Weight", ylab = "Miles Per Gallon", main = "MTCARS: MPG vs Weight")

help(plot)
install.packages("ggplot2")
library(ggplot2)

?ggplot

ggplot(mtcars, aes(x=wt, y=mpg))
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()


data("pressure")
View(pressure)
plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure)

plot(pressure$temperature, pressure$pressure, type = "b")
plot(pressure$temperature, pressure$pressure, type = "o")


lines(pressure$temperature, pressure$pressure/2,  col = "red")
points(pressure$temperature, pressure$pressure/2, col = "red")

lines(pressure$temperature, pressure$pressure*2,  col = "blue")
points(pressure$temperature, pressure$pressure*2, col = "blue")

ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()
ggplot(pressure,aes(x=temperature,y=pressure))+geom_col()





