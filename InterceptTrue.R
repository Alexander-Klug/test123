library(glmnet)
set.seed(123)
X <- matrix(c(-1, -4, 0, 0, 1, 16), nrow = 3, ncol = 2, byrow = TRUE)
y <- matrix(c(-2.2, 0, 3.8), nrow = 3, ncol = 1, byrow = TRUE)

# Define RMS function for standardization
rmsN <- function(x) {
  sqrt(mean((x - mean(x))^2)) # Computes Root Mean Square (RMS)
}

# Standardize X
X_centered <- sweep(X, 2, colMeans(X), "-") # Center the columns of X
X_scaled <- sweep(X_centered, 2, apply(X, 2, rmsN), "/") # Scale the columns of X

# Fit glmnet model with manually standardized X and original y
fit_manual <- glmnet(X_scaled, y, standardize = FALSE, intercept = FALSE, lambda = 0.1, maxit = 10000, family = "gaussian")

# Extract standardized coefficients
beta_standardized <- fit_manual$beta

# Rescale coefficients to the original scale using RMS
beta_original <- beta_standardized * (1 / apply(X, 2, rmsN))

# Print results
print("Coefficients on original scale:")
print(beta_original)

# Compare with glmnet's automatic standardization (intercept = FALSE)
fit_auto <- glmnet(X, y, standardize = TRUE, intercept = TRUE, lambda = 0.1, maxit = 10000, family = "gaussian")
print("Coefficients from glmnet's automatic standardization (intercept = FALSE):")
print(fit_auto$beta)
