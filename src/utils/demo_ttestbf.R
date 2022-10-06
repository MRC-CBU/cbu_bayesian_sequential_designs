# Illustrate ttest functionality
library(BayesFactor)

# Set the seed
set.seed(123)


# Unpaired, d = 0.5 ###########################################################

# Simulate the two data groups
data1 <- rnorm(50, mean = 0, sd = 1)
data2 <- rnorm(50, mean = 0.5, sd = 1)

# Calculate ttestBF
result <- ttestBF(data1,data2,paired = FALSE)

# Get the BF numerically:
as.numeric(as.vector(result))

# Unpaired, d = 0 ############################################################

# Simulate the two data groups
data1 <- rnorm(500, mean = 0, sd = 1)
data2 <- rnorm(500, mean = 0, sd = 1)

# Calculate ttestBF
result <- ttestBF(data1,data2,paired = FALSE)

# Get the BF numerically:
as.numeric(as.vector(result))
|