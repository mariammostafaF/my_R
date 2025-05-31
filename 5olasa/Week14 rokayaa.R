library(ff)
library(ffbase)
library(ffbase2)
#R datasets are loaded entirely into your computer RAM.
#If your dataset is bigger than your available RAM, R will either fail to load it or become extremely slow.
#The ff package stores the data on your hard disk in binary format. 
#It then provides R with a mapping to the data.
setwd("D:/R_Data")#working directory 
shell("mkdir air_ffdf")
#set a temporary directory for the ff package in R.
options(fftempdir = "D:/R_Data/air_ffdf")
#saving the path in a variable to use later
dir_air = "D:/R_Data/air_ffdf"
#We will use this to measure the execution time of the following code, to see how much time it takes to load the dataset.
ptm <- proc.time()
#BEFORE you read the dataset make sure it is saved in the directory "e:/R_Data"
airline.ff <- read.table.ffdf(file="flights_sep_oct15.txt",
                              sep="," , VERBOSE = TRUE,
                              header=TRUE, next.rows = 100000, colClasses=NA)
#Execution time - we measure the user time: Time spent by the CPU on the current process (reading the dataset).
x_time <- proc.time() -ptm
x_time

#Now we want to calculate the size of the dataset file in Megabytes (MB)
#This function returns the size in bytes....we divide by 1024 to convert it to kilobytes then again to megabytes.
#The file size is 149 megabytes
file.size("flights_sep_oct15.txt")/1024/1024

#Now we want to see the file size after we saved the file as an 'ff' object
#The file size is now 0.5 megabytes.
object.size(airline.ff)
object.size(airline.ff)/1024/1024
#Alternative 
format(object.size(airline.ff),"Mb")

#list the name of all the files in the directory 'e:/R_Data/air_ffdf'
list.files(dir_air)
#count the number of files--> the dataset has been split and stored in 29 files.
#The ff function stores each column in the dataset seperately.
length(list.files(dir_air))
#Lets check the file that contains the year column.
#This accesses the YEAR column of your airline.ff object.
#This is the file name including the directory
filename(airline.ff$YEAR)
#This is only the file name
basename(filename(airline.ff$YEAR))

#you can obtain statistics on any column in the dataset...sych as the maximum distance of a flight
max(airline.ff$DISTANCE)
#This command converts the distance column from a numerical column to a categorical column.
#Where flights are categorized according to the range of distance that they are in .
ptm <- proc.time() #Lets also measure the time of this process
dist_cut = cut.ff(airline.ff$DISTANCE, breaks = c(0, 150, 300, 450, 600,
                                                  750, 900, 1050, 1200, 2000, 5000))

#We will create a table then convert it to a dataframe to see how many flights fall into each range.

flight_distance= table.ff(dist_cut)
flight_distance= as.data.frame(flight_distance)
View(flight_distance)
colnames(flight_distance)[2]= "number_of_flights"
colnames(flight_distance)[1]= "Distance_range"
x_time <- proc.time() -ptm
#The process took almost 39 seconds
x_time
View(flight_distance)


#lets rename the last three columns in the dataframe. since their names are currently not very clear
flight_distance$Distance_range <- as.character(flight_distance$Distance_range)
flight_distance$Distance_range[6:10] <- c("(750,1000]","(1000,1250]","(1250,1500]", "(1500,1750]", "(1750,2000]" )
View(flight_distance)

#Now lets plot this new dataframe
library(ggplot2)
ggplot(flight_distance)+
  aes(x=Distance_range, y= number_of_flights) +
  geom_col(width=1, position= position_dodge(0.9))



#Lets try another operation..where we combine two columns in a table
ptm <- proc.time()
t1.ff <- table.ff(airline.ff$ORIGIN_STATE_NM, airline.ff$DEST_STATE_NM)
x_time <- proc.time() -ptm
x_time #the process takes 0.28
format(object.size(t1.ff), "Kb")#the table size is 18.6kb


#Final note: consider the traditional way of loading the dataset without using the ffbase package
#Lets read the dataset the traditional way 
library(readr)
ptm <- proc.time()
airline_1 <- read_file("flights_sep_oct15.txt")
x_time <- proc.time() -ptm
x_time
format(object.size(airline_1), "Mb")#file size is 149 Mb...which means its better to use ffbase


#repeat the same operation
ptm <- proc.time()
t1 <- table(airline.ff$ORIGIN_STATE_NM, airline.ff$DEST_STATE_NM)
x_time <- proc.time() -ptm
x_time #the process takes 0.28
format(object.size(t1), "Kb")#the table size is 18.6kb
