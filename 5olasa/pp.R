library(parallel)

# Define a simple function that returns the square of a number
square_fun <- function(x) {
  Sys.sleep(0.5)  # Artificial delay to simulate work (0.5 seconds)
  x^2
}

numbers <- 1:10  # Numbers to process

# ----------------------------
# SERIAL EXECUTION
# ----------------------------
cat("Starting serial execution...\n")
start_serial <- Sys.time()
pt<-proc.time()
serial_results <- lapply(numbers, square_fun)  # Compute squares one by one
end_serial <- Sys.time()
endpt<-proc.time()
serial_time <- as.numeric(difftime(end_serial, start_serial, units = "secs"))
cat("Serial execution time:", serial_time, "seconds\n")
x_time<-endpt-pt
x_time
# ----------------------------
# PARALLEL EXECUTION
# ----------------------------
num_cores <- detectCores() - 1  # Number of cores to use (leave one free)
cat("Starting parallel execution on", num_cores, "cores...\n")
cl <- makeCluster(num_cores,port = 12000)  # Create cluster

start_parallel <- Sys.time()
ppt<-proc.time()
parallel_results <- parLapply(cl, numbers, square_fun)  # Compute squares in parallel
end_parallel <- Sys.time()
endppt<-proc.time()
parallel_time <- as.numeric(difftime(end_parallel, start_parallel, units = "secs"))
cat("Parallel execution time:", parallel_time, "seconds\n")
px_time<-endppt-ppt
px_time

stopCluster(cl)  # Stop the cluster

# Check results are the same
print(unlist(serial_results))
print(unlist(parallel_results))

# ----------------------------
# EASIER SQUARING
# ----------------------------
library(parallel)

numCores<-detectCores()-1

cl<-makeCluster(numCores)
time<-proc.time()
results<-parLapply(cl,1:10,function(x) x^2)
timespend<-proc.time()-time
timespend
stopCluster(cl)

print(results)


stime<-proc.time()
sq_fun<-function(x){
  return(x^2)
}
sq_fun(1:10)
stimespend<-proc.time()-stime
stimespend

# ----------------------------
# FIB
# ----------------------------
fib<-function(x){
  if(x<=2)
    return(1)
  return(fib(x-1)+fib(x-2))
}

stime<-proc.time()

result_seq<-lapply(1:6, function(x) fib(35))

stime_spent<-proc.time()-stime
print(stime_spent)

library(parallel)

numCores<-detectCores()-1
cl<-makeCluster(numCores)

clusterExport(cl,varlist = "fib")

ptime<-proc.time()

result_par<-parLapply(cl,1:6,function(x) fib(35))

ptime_spent<-proc.time()-ptime
print(ptime_spent)

stopCluster(cl)
