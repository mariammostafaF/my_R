# Load necessary libraries
library(ff)
library(ffbase)
library(parallel)
library(ggplot2)

# Set working directories
setwd("D:/R projects")
shell("mkdir air_ffdf")
options(fftempdir = "D:/R projects/air_ffdf")
dir_air <- "D:/R projects/air_ffdf"

# Load the dataset as ffdf object
ptm <- proc.time()
airline.ff <- read.table.ffdf(file = "flights_sep_oct15 .txt",
                              sep = ",",
                              VERBOSE = TRUE,
                              header = TRUE,
                              next.rows = 100000,
                              colClasses = NA)
load_time <- proc.time() - ptm
print(load_time)

# Check file sizes
print(format(object.size(airline.ff), "Mb"))
print(file.size("flights_sep_oct15 .txt") / 1024 / 1024)

# Basic statistics
print(max(airline.ff$DISTANCE))

# Cut distances into ranges
ptm <- proc.time()
dist_cut <- cut.ff(airline.ff$DISTANCE,
                   breaks = c(0, 150, 300, 450, 600, 750, 900,
                              1050, 1200, 2000, 5000))
flight_distance <- table.ff(dist_cut)
flight_distance <- as.data.frame(flight_distance)
colnames(flight_distance) <- c("Distance_range", "number_of_flights")
flight_distance$Distance_range <- as.character(flight_distance$Distance_range)
flight_distance$Distance_range[6:10] <- c("(750,1000]", "(1000,1250]",
                                          "(1250,1500]", "(1500,1750]", "(1750,2000]")
cut_time <- proc.time() - ptm
print(cut_time)

# Plot
ggplot(flight_distance) +
  aes(x = Distance_range, y = number_of_flights) +
  geom_col(width = 1, position = position_dodge(0.9)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ----------- Parallel processing for 2-way table -----------

# Detect cores
numCores <- detectCores() - 1
cl <- makeCluster(numCores)
clusterExport(cl, varlist = c("airline.ff"))

# Split indices
n <- nrow(airline.ff)
chunk_indices <- split(1:n, cut(1:n, numCores, labels = FALSE))

# Function to compute partial table
partial_table <- function(indices) {
  table(airline.ff$ORIGIN_STATE_NM[indices],
        airline.ff$DEST_STATE_NM[indices])
}

# Run in parallel
ptm <- proc.time()
partial_results <- parLapply(cl, chunk_indices, partial_table)
stopCluster(cl)

# Combine all partial tables
t1_combined <- Reduce("+", partial_results)
par_time <- proc.time() - ptm
print(par_time)
print(format(object.size(t1_combined), "Kb"))

# View result
print(head(t1_combined))

# Traditional (non-parallel) method for comparison
ptm <- proc.time()
t1_single <- table(airline.ff$ORIGIN_STATE_NM,
                   airline.ff$DEST_STATE_NM)
single_time <- proc.time() - ptm
print(single_time)
print(format(object.size(t1_single), "Kb"))
