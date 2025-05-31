# Load required libraries
library(parallel)  # For parallel processing
library(ggplot2)   # For plotting

# Load data
data("diamonds")

# Define analysis function
# This function fits a linear model excluding the current row,
# predicts the price for the current row, and calculates the absolute error.
analyze_diamond <- function(row_data) {
  # Step 1: Exclude the current row to create a training dataset
  train_data <- diamonds[rownames(diamonds) != rownames(row_data), ]
  
  # Step 2: Fit a linear regression model to predict price using several predictors
  model <- lm(price ~ carat + cut + color + clarity, data = train_data)
  
  # Step 3: Predict the price for the excluded (test) row
  pred <- predict(model, newdata = row_data)
  
  # Step 4: Return the absolute error between predicted and actual price
  abs(pred - row_data$price)
}

# Number of cores (leave 1 core free for system stability)
num_cores <- detectCores() - 1

# Sample sizes to test performance on
sizes <- seq(10, 100, by = 10)

# Data frame to store benchmarking results
results <- data.frame(
  SampleSize = integer(),
  SerialTime = numeric(),
  ParallelTime = numeric(),
  TimeSaved = numeric(),
  PercentSaved = numeric()
)

# Create a socket cluster for parallel processing using available cores
cl <- makeCluster(num_cores)

# Export necessary data and functions to each worker in the cluster
clusterExport(cl, varlist = c("diamonds", "analyze_diamond"))

# Main benchmarking loop over different sample sizes
for (size in sizes) {
  cat("Processing sample size:", size, "\n")
  
  # Split first 'size' rows of diamonds dataset into a list of single-row data.frames
  diamond_samples <- split(diamonds[1:size, ], 1:size)
  
  # ----------------------------
  # SERIAL EXECUTION
  # ----------------------------
  start_serial <- Sys.time()  # Start time for serial run
  serial_results <- lapply(diamond_samples, analyze_diamond)  # Apply function sequentially
  end_serial <- Sys.time()    # End time for serial run
  serial_time <- as.numeric(difftime(end_serial, start_serial, units = "secs"))
  
  # ----------------------------
  # PARALLEL EXECUTION
  # ----------------------------
  start_parallel <- Sys.time()  
  parallel_results <- parLapply(cl, diamond_samples, analyze_diamond)  # Apply function in parallel
  end_parallel <- Sys.time()
  parallel_time <- as.numeric(difftime(end_parallel, start_parallel, units = "secs"))
  
  # Calculate time saved and percentage time saved by parallelization
  time_saved <- serial_time - parallel_time
  percent_saved <- (time_saved / serial_time) * 100
  
  # Store benchmarking results in the data frame
  results <- rbind(results, data.frame(
    SampleSize = size,
    SerialTime = serial_time,
    ParallelTime = parallel_time,
    TimeSaved = time_saved,
    PercentSaved = percent_saved
  ))
  
  print(results)
}

# Stop the cluster after benchmarking
stopCluster(cl)

# Plot the percent time saved vs sample size
ggplot(results, aes(x = SampleSize)) +
  geom_line(aes(y = PercentSaved), color = "blue", linewidth = 1.2) +
  geom_point(aes(y = PercentSaved), color = "blue", size = 2) +
  labs(
    title = "Parallel Time Saving vs Sample Size",
    x = "Sample Size",
    y = "Time Saved (%)"
  )



# ------------------------------------------------
# Benchmarking effect of number of cores on parallel time for fixed sample size (100)
# ------------------------------------------------

library(parallel)
library(ggplot2)

data("diamonds")

# Re-define analysis function to ensure availability
analyze_diamond <- function(row_data) {
  train_data <- diamonds[rownames(diamonds) != rownames(row_data), ]
  model <- lm(price ~ carat + cut + color + clarity, data = train_data)
  pred <- predict(model, newdata = row_data)
  abs(pred - row_data$price)
}

# Split first 100 rows into single-row data frames for processing
diamond_samples_100 <- split(diamonds[1:100, ], 1:100)

# Range of cores to test: from 1 to number of available cores minus 1
core_range <- 1:(detectCores() - 1)

# Data frame to store parallel execution time for different core counts
core_results <- data.frame(Cores = integer(), ParallelTime = numeric())

# Loop over number of cores
for (c in core_range) {
  cl <- makeCluster(c)  # Create cluster with c cores
  clusterExport(cl, varlist = c("diamonds", "analyze_diamond"))  # Export necessary data & functions
  
  start_core <- Sys.time()  # Start timing
  parLapply(cl, diamond_samples_100, analyze_diamond)  # Parallel execution
  end_core <- Sys.time()    # End timing
  
  stopCluster(cl)  # Stop the cluster after each run
  
  ptime <- as.numeric(difftime(end_core, start_core, units = "secs"))
  core_results <- rbind(core_results, data.frame(Cores = c, ParallelTime = ptime))
  
  cat(sprintf("Cores: %d | Parallel time: %.2fs\n", c, ptime))
}

# Plot parallel execution time vs number of cores used
ggplot(core_results, aes(x = Cores, y = ParallelTime)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_point(color = "darkgreen", size = 2) +
  labs(
    title = "Parallel Time vs Number of Cores (100 Samples)",
    x = "Number of Cores",
    y = "Parallel Execution Time (s)"
  )
