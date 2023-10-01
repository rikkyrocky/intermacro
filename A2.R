# Additional libraries needed
library(dplyr)

# Load the dataset
data("pwt10.01")

# Define the countries of interest and initialize a list to store plots
countries <- c("CAN", "DNK", "IRL", "TWN", "IND", "KEN", "NIC", "NGA")
plots_list <- list()

# Calculate the rGDPpp for the United States
us_rgdpo <- pwt10.01[pwt10.01$isocode == "USA", c("rgdpo")]
us_pop <- pwt10.01[pwt10.01$isocode == "USA", c("pop")]
us_rGDPpp <- us_rgdpo/us_pop

# Loop over the countries of interest to calculate the ratio and create plots
for (country in countries) {
  country_data <- pwt10.01[pwt10.01$isocode == country, ]
  country_rgdpo <- country_data$rgdpo
  country_pop <- country_data$pop
  country_rGDPpp <- country_rgdpo/country_pop
  
  # Ensure that lengths are the same before calculating the ratio
  min_length <- min(length(country_rGDPpp), length(us_rGDPpp))
  ratio <- country_rGDPpp[1:min_length] / us_rGDPpp[1:min_length]
  
  # Create time series object and plot
  start_year <- country_data$year[1]
  ratio_ts <- ts(ratio, start = start_year, frequency = 1)
  plot_obj <- plot(ratio_ts, main = paste("rGDPpp Ratio of", country, "to USA"), ylab = "rGDPpp Ratio", xlab = "Year", type = "l")
  
  plots_list[[country]] <- plot_obj
}

# Display the plots
plots_list




#Question 2
# Defining Parameters
alpha <- 1/4
gA <- 0.02
gN <- 0.02
delta <- 0.05
s1 <- 0.12
s2 <- 0.17
time_periods <- 100

# Initializing Vectors to store calculated values
k <- numeric(time_periods+1)
y <- numeric(time_periods)
c <- numeric(time_periods)
A <- numeric(time_periods)

# Initial condition
k[1] <- 1.468 # assuming an arbitrary initial capital per effective worker
A[1] <- 1
# Calculations for the time-paths
for (t in 1:time_periods) {
  s <- ifelse(t == 1, s1, s2)
  k[t+1] <- ((1 + gA + gN) * k[t] + s * k[t]^alpha - (delta + gA + gN) * k[t]) / (1 + gA + gN)
  y[t] <- k[t]^alpha
  
  if (t > 1) {
    A[t] <- A[t-1]*(1+gA)
  }
  c[t] <- (1 - s) * y[t]
}
print(A[50])
# Plotting
par(mfrow = c(2,2))
plot(0:(time_periods - 1), k[1:time_periods], type="l", col="blue", main="Capital per Effective Worker", xlab="Time", ylab="k(t)")
plot(0:(time_periods - 1), y, type="l", col="red", main="Output per Effective Worker", xlab="Time", ylab="y(t)")
plot(0:(time_periods - 1), c, type="l", col="green", main="Consumption per Effective Worker", xlab="Time", ylab="c(t)")

# Calculating log output and log consumption per worker
log_y_worker <- log(y*A, 2.718)
print(y*A)
log_c_worker <- log(c *A, 2.718)
print(log_c_worker[50])
print(log_y_worker[50])

plot(0:(time_periods - 1), log_y_worker, type="l", col="purple", main="Log Output per Worker", xlab="Time", ylab="log(Y/L)")
plot(0:(time_periods - 1), log_c_worker, type="l", col="orange", main="Log Consumption per Worker", xlab="Time", ylab="log(C/L)")


#question 3 
# Defining Parameters

alpha <- 1/4
gA <- 0.025
gN <- 0.02
delta <- 0.05
s1 <- 0.12
s2 <- 0.12
time_periods <- 100

# Initializing Vectors to store calculated values
k <- numeric(time_periods+1)
y <- numeric(time_periods)
c <- numeric(time_periods)
A <- numeric(time_periods)

# Initial condition
k[1] <- 1.468 
A[1] <- 1
# Calculations for the time-paths
for (t in 1:time_periods) {
  s <- ifelse(t == 1, s1, s2)
  k[t+1] <- ((1 + gA + gN) * k[t] + s*k[t]^alpha - (delta + gA + gN) * k[t]) / (1 + gA + gN)
  y[t] <- k[t]^alpha
  c[t] <- (1 - s) * y[t]
  if (t > 1) {
    A[t] <- A[t-1]*(1+gA)
  }
}


# Plotting
par(mfrow = c(2,2))
plot(0:(time_periods - 1), k[1:time_periods], type="l", col="blue", main="Capital per Effective Worker", xlab="Time", ylab="k(t)")
plot(0:(time_periods - 1), y, type="l", col="red", main="Output per Effective Worker", xlab="Time", ylab="y(t)")
plot(0:(time_periods - 1), c, type="l", col="green", main="Consumption per Effective Worker", xlab="Time", ylab="c(t)")

# Calculating log output and log consumption per worker
log_y_worker1 <- log(y*A, 2.718)
log_c_worker <- log(c*A, 2.718)
#print(y*A)


plot(0:(time_periods - 1), log_y_worker1, type="l", col="purple", main="Log Output per Worker", xlab="Time", ylab="log(Y/L)")
lines(0:(time_periods - 1), log_y_worker, col="blue", lty=2)



legend("topright",           
       legend=c("Old gA", "New gA"),   
       col=c("blue", "purple"),    
       lty=c(2, 1),          
       cex=0.8) 
plot(0:(time_periods - 1), log_c_worker, type="l", col="orange", main="Log Consumption per Worker", xlab="Time", ylab="log(C/L)")
#question 3 
# Defining Parameters

alpha <- 1/4
gA <- 0.02
gN <- -0.01
delta <- 0.05
s1 <- 0.12
s2 <- 0.12
time_periods <- 100

# Initializing Vectors to store calculated values
k <- numeric(time_periods+1)
y <- numeric(time_periods)
c <- numeric(time_periods)
A <- numeric(time_periods)

# Initial condition
k[1] <- 1.468 
A[1] <- 1
# Calculations for the time-paths
for (t in 1:time_periods) {
  s <- ifelse(t == 1, s1, s2)
  k[t+1] <- ((1 + gA + gN) * k[t] + s*k[t]^alpha - (delta + gA + gN) * k[t]) / (1 + gA + gN)
  y[t] <- k[t]^alpha
  c[t] <- (1 - s) * y[t]
  if (t > 1) {
    A[t] <- A[t-1]*(1+gA)
  }
}

# Plotting
par(mfrow = c(2,2))
plot(0:(time_periods - 1), k[1:time_periods], type="l", col="blue", main="Capital per Effective Worker", xlab="Time", ylab="k(t)")
plot(0:(time_periods - 1), y, type="l", col="red", main="Output per Effective Worker", xlab="Time", ylab="y(t)")
plot(0:(time_periods - 1), c, type="l", col="green", main="Consumption per Effective Worker", xlab="Time", ylab="c(t)")

# Calculating log output and log consumption per worker
log_y_worker2 <- log(y*A, 2.718)
log_c_worker <- log(c*A, 2.718)
print(y*A)
print(log_y_worker)
print(log_y_worker2)




plot(0:(time_periods - 1), log_y_worker2, type="l", col="purple", main="Log Output per Worker", xlab="Time", ylab="log(Y/L)")
lines(0:(time_periods - 1), log_y_worker, col="blue", lty=2)
lines(0:(time_periods - 1), log_y_worker1, col="red", lty=2)



legend("topright",           
       legend=c("S", "gA", "gN"),   
       col=c("blue", "red", "purple"),    
       lty=c(2,2, 1),          
       cex=0.8) 
plot(0:(time_periods - 1), log_c_worker, type="l", col="orange", main="Log Consumption per Worker", xlab="Time", ylab="log(C/L)")