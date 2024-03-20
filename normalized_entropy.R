## Normalized entropy

# Load the required libraries
library(dplyr)
library(purrr)

# Define a function for entropy
entropy <- function(x) {
  prob <- table(x) / length(x)
  -sum(prob * log2(prob))
}

# Define a function for normalized entropy
normalized_entropy <- function(x) {
  entropy(x) / log2(length(unique(x)))
}

# Load the mtcars dataset
data(mtcars)

# Calculate the normalized entropy
mtcars %>%
  pull(cyl) %>% 
  normalized_entropy()

####




# Define function to calculate binary entropy
binary_entropy <- function(p) {
  # Handle case of p = 0 or p = 1
  p <- ifelse(p == 0, 1e-7, p)
  p <- ifelse(p == 1, 1 - 1e-7, p)
  return(-p*log2(p) - (1-p)*log2(1-p))
}

# Define function to calculate cross entropy
cross_entropy <- function(y, p_hat) {
  # Handle case of p_hat = 0 or p_hat = 1
  p_hat <- ifelse(p_hat == 0, 1e-7, p_hat)
  p_hat <- ifelse(p_hat == 1, 1 - 1e-7, p_hat)
  return(-y*log2(p_hat) - (1-y)*log2(1-p_hat))
}

# Define function to calculate normalized entropy
normalized_entropy <- function(y, p_hat) {
  ce <- sum(cross_entropy(y, p_hat), na.rm = TRUE)
  be <- sum(binary_entropy(p_hat), na.rm = TRUE)
  return(ce / be)
}

# Generate synthetic data
set.seed(123) # for reproducibility
n_samples <- 100
y <- rbinom(n_samples, 1, 0.5) # true class labels
p_hat <- runif(n_samples) # prediction probabilities

# Calculate normalized entropy
ne <- normalized_entropy(y, p_hat)
print(ne)

