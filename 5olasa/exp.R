# Load required packages
library(ff)
library(ffbase)
library(parallel)
library(ggplot2)
library(readr)

# Set working and temp directories
setwd("D:/R projects")
shell("mkdir dir")
options(fftempdir = "D:/R projects/dir")
dir_air <- "D:/R projects/dir"

# Load data using ffdf
ptm <- proc.time()
airline.ff <- read.table.ffdf(file = "flights_sep_oct15 .txt",  # watch the space
                              sep = ",", VERBOSE = TRUE,
                              header = TRUE, next.rows = 100000, colClasses = NA)
x_time <- proc.time() - ptm
print(x_time)

# File size comparison
print(file.size("flights_sep_oct15 .txt") / 1024 / 1024)  # in MB
print(format(object.size(airline.ff), "Mb"))
print(length(list.files(dir_air)))

# Setup parallel processing
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(ff)
  library(ffbase)
})

# Create row index chunks
row_chunks <- chunk(1:nrow(airline.ff), by = n_cores)

# ================================
# PART 1: FLIGHT DISTANCE ANALYSIS
# ================================

# Export data for parallel chunking
clusterExport(cl, varlist = c("airline.ff"))

# Parallel: cut + table.ff for distance
ptm <- proc.time()
results <- parLapply(cl, row_chunks, function(rows) {
  dist_cut_chunk <- cut.ff(airline.ff$DISTANCE[rows],
                           breaks = c(0, 150, 300, 450, 600,
                                      750, 900, 1050, 1200, 2000, 5000))
  table.ff(dist_cut_chunk)
})
x_time <- proc.time() - ptm
print(x_time)

# Combine results
flight_distance <- Reduce("+", results)
flight_distance_df <- as.data.frame(flight_distance)
colnames(flight_distance_df) <- c("Distance_range", "number_of_flights")
flight_distance_df$Distance_range <- as.character(flight_distance_df$Distance_range)
flight_distance_df$Distance_range[6:10] <- c("(750,1000]",
                                             "(1000,1250]",
                                             "(1250,1500]",
                                             "(1500,1750]",
                                             "(1750,2000]")

# Plot distance distribution
ggplot(flight_distance_df) +
  aes(x = Distance_range, y = number_of_flights) +
  geom_col(width = 1, position = position_dodge(0.9), fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Flight Count by Distance Range", x = "Distance Range", y = "Number of Flights")

