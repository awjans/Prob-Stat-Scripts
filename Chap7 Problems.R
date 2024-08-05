# Central Limit Theorem
y <- c()
# Examples of some basic statistics
# Sample Size
n <- length(y)
# Sample Mean
ybar <- (1/n)*sum(y)
# Sample Variance
s2 <- (1/(n-1))*sum((y-ybar)^2)
# Sample Standard Deviation
s <- sqrt((1/(n-1))*sum((y-ybar)^2))
s <- sqrt(s2)
# Sample Range
range <- max(y) - min(y)

# Normal Distribution
# Ybar ~ N(mu, s2/n)
# Convert from Normal to Standard Normal
z <- (ybar - mu)/(s/sqrt(n))
# Convert from Standard Normal to Normal
ybar <- z*(s/sqrt(n)) + mu

# Chi-Squared Distribution
# Sum of Squares of n Independent Standard Normal Variables
c2 <- sum(z^2)
z <- (y - mu)/s
z2 <- sum(((y - mu)/s)^2)
t <- (y - mu)/(s/sqrt(n))

# F-Distribution
# Ratio of Two Independent Chi-Squared Variables
f <- (s1^2/sigma1^2)/(s2^2/sigma2^2)

# Central Limit Theorem
# If Y1, Y2, ..., Yn are independent and identically distributed random variables with mean mu and variance sigma^2, then the distribution of the sample mean Ybar approaches a normal distribution with mean mu and variance sigma^2/n as n approaches infinity.
# Ybar ~ N(mu, sigma^2/n)
u <- (ybar - mu)/(s/sqrt(n))
limP <- integrate(function(t) ((1/sqrt(2*pi))*exp(-t^2/2)), -Inf, u)

# Poisson Distribution
# Number of Events in a Fixed Interval of Time
# E(Y) = Var(Y) = lambda
# Y ~ Poisson(lambda)
# E(Y) = lambda
# Var(Y) = lambda
# Sigma(Y) = sqrt(lambda)

# Exponential Distribution
# Time Between Events in a Poisson Process
# E(Y) = 1/lambda
# Var(Y) = 1/lambda^2
# Sigma(Y) = 1/lambda

# Estimation
# Goal of Statistics: use the information collected in data to make inference (draw conclusion) about the population from which the sample is taken.
# Estimator: is a rule, or formula, that tells how to calculate the value on an estimation using the data.
# Point Estimator
ybar <- (1/n)*sum(y)
# Interval Estimator
ybar <- t*s/sqrt(n) # T-Statistic Estimator
ybar <- z*s/sqrt(n) # Z-Statistic Estimator

# Bias and Mean Square Error
# Bias: the difference between the expected value of an estimator and the true value of the parameter being estimated.
theta = E(theta-hat) # Unbiased (Good)
theta != E(theta-hat) # Biased
# Mean Square Error: the expected value of the square of the difference between the estimator and the true value of the parameter being estimated.
MSE = E((theta-hat - theta)^2)