# Load necessary library
set.seed(123)

# Simulate some data ####
n <- 200
x <- runif(n, 0, 10)

# Define a polynomial relationship for a continuous outcome
# Example: y = 0.5*x - 0.3*x^2 + 0.1*x^3 + some noise
y <- 0.5*x - 0.3*x^2 + 0.1*x^3 + rnorm(n, 0, 2)  # adding some random noise

# Fit a polynomial regression model ####
fit <- lm(y ~ poly(x, 4))

# Summary of the model
summary(fit)

# Predict on a sequence of x values
x_new <- seq(min(x), max(x), length.out = 200)
y_pred <- predict(fit, newdata = data.frame(x = x_new))

# Plot the original data and the fitted curve ####
plot(x, y, main = "Polynomial Regression", col = "red", pch = 19)
lines(x_new, y_pred, col = "blue", lwd = 2)

# Find appropriate degree ####
# Define a function to perform cross-validation
library(boot)

cv_error <- sapply(1:10, function(degree) {
  model <- glm(y ~ poly(x, degree))
  cv.glm(data.frame(x, y), model, K = 10)$delta[1]
})

# Find the degree with the minimum cross-validation error
best_degree <- which.min(cv_error)
best_degree
