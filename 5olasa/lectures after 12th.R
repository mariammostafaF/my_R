############################################
#Week13              PART1
############################################
df<-data.frame(ID=1:5,
               Gender=c("Male","Female","Female","Male","Male"),
               Age=c(25,30,22,45,35),
               Income=c(50000,60000,45000,80000,70000),
               Employee=c(TRUE,TRUE,FALSE,TRUE,FALSE),
               Satisfaction=factor(c("High","Low","Medium","High","Low")))
View(df)
str(df)
df$Gender=as.numeric(as.factor(df$Gender))
df$Employee=as.numeric(df$Employee)
df$Satisfaction=as.numeric(df$Satisfaction)
str(df)

numeric_matrix<-as.matrix(df)
numeric_matrix

colMeans(numeric_matrix)
colSums(numeric_matrix)

apply(numeric_matrix,2,sd)
apply(numeric_matrix,2,var)
apply(numeric_matrix,2,min)
apply(numeric_matrix,2,max)
apply(numeric_matrix,2,median)
apply(numeric_matrix,2,range)
apply(numeric_matrix,2,quantile)
apply(numeric_matrix,2,mean)
apply(numeric_matrix,2,sum)
apply(numeric_matrix,2,summary)

cor(numeric_matrix)
cov(numeric_matrix)


install.packages("bigmemory")
library(bigmemory)
big_mat<-as.big.matrix(numeric_matrix)
big_mat
big_mat[]

object.size(numeric_matrix)
object.size(big_mat)

colmean<-colMeans(big_mat[])
colmean

column_means<-apply(big_mat[], 2,mean)
column_means

column_sd<-apply(big_mat[], 2,sd)
column_sd


############################################
#Week13              PART2
############################################
#install.packages("ff")
#install.packages("ffbase")
library(ff)
library(ffbase)

setwd("D:/R projects")
getwd()

#Creates a new folder called air_ffdf inside D:/R projects using a Windows shell command
shell("mkdir air_ffdf") 

#Tells the ff package to store temporary files in the folder you just created
#These files are used when loading and processing big data
options(fftempdir = "D:/R projects/air_ffdf")

#Saves the path into a variable called dir_air for reuse
dir_air="D:/R projects/air_ffdf"

#Starts a timer to measure how long it takes to load the data
ptm<-proc.time()

#Loads the CSV file into an ffdf object (air.ff), 
#which allows data to be processed chunk-by-chunk from disk instead of loading the whole thing into RAM
air.ff<-read.table.ffdf(file = "flights_sep_oct15 .txt",sep=",",VERBOSE = TRUE,
                        header=TRUE,next.rows = 100000,colClasses=NA)

#Measures and displays how long the data load took
x_time<-proc.time()-ptm
x_time


###################################################################
#week14
###################################################################
#Returns the file size of the raw .txt file, in bytes and megabytes
file.size("flights_sep_oct15 .txt")
file.size("flights_sep_oct15 .txt")/1024/1024 #kilo mega

#Checks the memory used by air.ff (only metadata, not full data, since ffdf stores most on disk)
object.size(air.ff)/1024/1024
format(object.size(air.ff),"Mb")

#Lists the files in the temporary directory used by ff, and counts how many there are (likely one per column)
list.files(dir_air)
length(list.files(dir_air))

#Shows the filename on disk for the YEAR column — ff stores each column in a separate binary file
basename(filename(air.ff$YEAR))

#install.packages("readr")
library(readr)
#Loads the entire file as a single character string using readr. This uses a lot of memory
airline_1<-read_file("flights_sep_oct15 .txt")
object.size(air.ff)
object.size(airline_1)

#Compares memory usage of ffdf version (disk-based) and the full text version (RAM-based)
format(object.size(air.ff),"Mb")
format(object.size(airline_1),"Mb")

#install.packages("devtools")
library(devtools)
# Install latest ffbase from GitHub
#install_github("edwindj/ffbase", subdir = "pkg", force = TRUE)
library(ffbase)

#data manipulation
#Creates a cross-tabulation (frequency table) of origin state vs destination state using ff objects
t1.ff<-table.ff(air.ff$ORIGIN_STATE_NM,air.ff$DEST_STATE_NM)
t1.ff
max(air.ff$DISTANCE)
t1<-table(air.ff$ORIGIN_STATE_NM,air.ff$DEST_STATE_NM)
t1
#Creates bins (categories) of distance
air.ff<-cut.ff(air.ff$DISTANCE,breaks=c(0,250,500,750,1000,1250,1500,1750,2000))
air.ff<-as.data.frame(air.ff)
dist_cut

library(ggplot2)
ggplot(air.ff,aes(x=DISTANCE))+geom_point()

