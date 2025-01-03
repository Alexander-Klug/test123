library(glmnet)
set.seed(123)

# Input Data
X <- matrix(c(-1, -4, 0, 0, 1, 16), nrow = 3, ncol = 2, byrow = TRUE)
y <- matrix(c(-2.2, 0, 3.8), nrow = 3, ncol = 1, byrow = TRUE)

# Standardization function using 1/N
rmsN <- function(x) {
  sqrt(mean((x - mean(x))^2)) # Computes Root Mean Square (RMS)
}

# Scale X without centering
X_scaled <- sweep(X, 2, apply(X, 2, rmsN), "/")

# Fit glmnet with manually scaled X and uncentered y
fit_manual <- glmnet(X_scaled, y, standardize = FALSE, intercept = FALSE, lambda = 0.1, maxit = 10000, family = "gaussian", thresh = 1e-07)

# Extract coefficients from the standardized model
beta_standardized <- fit_manual$beta

# Rescale coefficients to the original scale
beta_original <- beta_standardized * (1 / apply(X, 2, rmsN))

# Print results
print("Manually computed coefficients:")
print(beta_original)

# Compare with glmnet's automatic standardization (intercept = FALSE)
fit_auto <- glmnet(X, y, standardize = TRUE, intercept = FALSE, lambda = 0.1, maxit = 10000, family = "gaussian", thresh = 1e-07)
print("Coefficients from glmnet's automatic standardization:")
print(fit_auto$beta)



