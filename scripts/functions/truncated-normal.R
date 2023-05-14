
library(msm)

n <- 100000
mean <- 0.5
sd <- 1
lower <- 0
upper <- Inf

data <- msm::rtnorm(1, lower=((lower - mean)/sd), upper=((upper - mean)/sd))
while (length(data) < n) {
  sample <- msm::rtnorm(1, lower=((lower - mean)/sd), upper=((upper - mean)/sd))
  data_copy = c(data, sample)
  data_copy_scaled = mean + sd * scale(data_copy)
  if (min(data_copy_scaled) >= lower & max(data_copy_scaled) <= upper) {
    data = c(data, sample)
  }
}

scaled_data = as.numeric(mean + sd * scale(data))

summary(scaled_data)
sd(scaled_data)
hist(scaled_data)
