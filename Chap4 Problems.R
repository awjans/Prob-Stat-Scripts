# Example 1: When a certain car breaks down, the time that it takes to fix it (in hours) is a random variable with the density function f(y) = ce^(-3y) if y >= 0.
f <- function(y) c*exp(-3*y)
# a) Calculate the value of c.
c <- integrate(f, 0, Inf)
c$value
# b) Find the probability that when this car breaks down, it takes at most 30 minutes to fix it.
p <- integrate(f, 0, 0.5)
p$value

# Example 14: The lifetime of a TV tube (in years) is an exponential random variable with mean 10. If Emma bought her TV set 10 years ago, what is the probability that its tube will last another 10 years?
x <- pexp(10, rate = 1/10)

# Example 15: Guests arrive at a hotel, in accordance with a Poisson process, at a rate of five per hour. Suppose that for the last 10 minutes no guest has arrived. What is the probability that that
# a) the next one will arrive in less than 2 minutes
p1 <- ppois(1, 5/60*2)
# b) from the arrival of the tenth to the arrival of the eleventh guest takes no more than 2 minutes?
p2 <- ppois(11, 10, lower.tail = TRUE) - ppois(10, 10, lower.tail = TRUE)

Mean <- mu <- E(Y) <- integrate(function(y) y*f(y), -Inf, Inf)
Var(Y) <- integrate(function(y) (y-mu)^2*f(y), -Inf, Inf)
Var(Y) <- E(Y^2) - mu^2

# Uniform Distribution
Uniform <- function(a, b) {
  E <- (a+b)/2
  Var <- (b-a)^2/12
  Sigma <- sqrt((b-a)^2/12)
  return(list(E = E, Var = Var, Sigma = Sigma))
}
# E(U(a,b)) = (a+b)/2
# Var(U(a,b)) = (b-a)^2/12
# Sigma(U(a,b)) = sqrt((b-a)^2/12)

# Normal (Gaussian) Distribution
# E(N(mu,sigma)) = mu
# Var(N(mu,sigma)) = sigma^2
# Sigma(N(mu,sigma)) = sigma
Normal <- function(mu, sigma) {
  E <- mu
  Var <- sigma^2
  Sigma <- sigma
  return(list(E = E, Var = Var, Sigma = Sigma))
}

# Convert from Normal Distribution to Standard Distribution
# Z = (X - mu)/sigma

# Binomial Distribution
# E(B(n,p)) = np
# Var(B(n,p)) = np(1-p)
# Sigma(B(n,p)) = sqrt(np(1-p))
Binomial <- function(n, p) {
  E <- n*p
  Var <- n*p*(1-p)
  Sigma <- sqrt(n*p*(1-p))
  return(list(E = E, Var = Var, Sigma = Sigma))
})

# Gamma Distribution
# E(Gamma(alpha,beta)) = alpha/beta
# Var(Gamma(alpha,beta)) = alpha/beta^2
# Sigma(Gamma(alpha,beta)) = sqrt(alpha)/beta
Gamma <- function(alpha, beta) {
  E <- alpha/beta
  Var <- alpha/beta^2
  Sigma <- sqrt(alpha)/beta
  return(list(E = E, Var = Var, Sigma = Sigma))
}

# Exponential Distribution
# E(Exp(lambda)) = 1/lambda
# Var(Exp(lambda)) = 1/lambda^2
# Sigma(Exp(lambda)) = 1/lambda
Exponential <- function(lambda) {
  E <- 1/lambda
  Var <- 1/lambda^2
  Sigma <- 1/lambda
  return(list(E = E, Var = Var, Sigma = Sigma))
}

# Poisson Distribution
# E(Poisson(lambda)) = lambda
# Var(Poisson(lambda)) = lambda
# Sigma(Poisson(lambda)) = sqrt(lambda)
Poisson <- function(lambda) {
  E <- lambda
  Var <- lambda
  Sigma <- sqrt(lambda)
  return(list(E = E, Var = Var, Sigma = Sigma))
}

# Geometric Distribution
# E(Geometric(p)) = 1/p
# Var(Geometric(p)) = (1-p)/p^2
# Sigma(Geometric(p)) = sqrt((1-p)/p^2)
Geometric <- function(p) {
  E <- 1/p
  Var <- (1-p)/p^2
  Sigma <- sqrt((1-p)/p^2)
  return(list(E = E, Var = Var, Sigma = Sigma))
}

# Hypergeometric Distribution
# E(Hypergeometric(N,M,n)) = n*M/N
# Var(Hypergeometric(N,M,n)) = n*M*(N-M)*(N-n)/(N^2*(N-1))
# Sigma(Hypergeometric(N,M,n)) = sqrt(n*M*(N-M)*(N-n)/(N^2*(N-1)))
Hypergeometric <- function(N, M, n) {
  E <- n*M/N
  Var <- n*M*(N-M)*(N-n)/(N^2*(N-1))
  Sigma <- sqrt(n*M*(N-M)*(N-n)/(N^2*(N-1))
  return(list(E = E, Var = Var, Sigma = Sigma))
}

# Negative Binomial Distribution
# E(NB(r,p)) = r/p
# Var(NB(r,p)) = r(1-p)/p^2
# Sigma(NB(r,p)) = sqrt(r(1-p)/p^2)
NegativeBinomial <- function(r, p) {
  E <- r/p
  Var <- r(1-p)/p^2
  Sigma <- sqrt(r(1-p)/p^2)
  return(list(E = E, Var = Var, Sigma = Sigma))
}

# Chi-Square Distribution
# E(Chi-Square(n)) = n
# Var(Chi-Square(n)) = 2n
# Sigma(Chi-Square(n)) = sqrt(2n)
ChiSquare <- function(n) {
  E <- n
  Var <- 2n
  Sigma <- sqrt(2n)
  return(list(E = E, Var = Var, Sigma = Sigma))
}

# Student's t Distribution
# E(t(n)) = 0
# Var(t(n)) = n/(n-2) for n > 2
# Sigma(t(n)) = sqrt(n/(n-2))
StudentT <- function(n) {
  E <- 0
  Var <- n/(n-2)
  Sigma <- sqrt(n/(n-2))
  return(list(E = E, Var = Var, Sigma = Sigma))
}

# F Distribution
# E(F(m,n)) = n/(n-2) for n > 2
# Var(F(m,n)) = 2n^2*(m+n-2)/(m*(n-2)^2*(n-4)) for n > 4
# Sigma(F(m,n)) = sqrt(2n^2*(m+n-2)/(m*(n-2)^2*(n-4))) for n > 4
FDistribution <- function(m, n) {
  E <- n/(n-2)
  Var <- 2n^2*(m+n-2)/(m*(n-2)^2*(n-4))
  Sigma <- sqrt(2n^2*(m+n-2)/(m*(n-2)^2*(n-4))
  return(list(E = E, Var = Var, Sigma = Sigma))
}

# Weibull Distribution
# E(Weibull(alpha,beta)) = beta*gamma(1+1/alpha)
# Var(Weibull(alpha,beta)) = beta^2*(gamma(1+2/alpha) - gamma(1+1/alpha)^2)
# Sigma(Weibull(alpha,beta)) = sqrt(beta^2*(gamma(1+2/alpha) - gamma(1+1/alpha)^2))

# Lognormal Distribution
# E(Lognormal(mu,sigma)) = exp(mu + sigma^2/2)
# Var(Lognormal(mu,sigma)) = (exp(sigma^2) - 1)*exp(2*mu + sigma^2)
# Sigma(Lognormal(mu,sigma)) = sqrt((exp(sigma^2) - 1)*exp(2*mu + sigma^2))

# Beta Distribution
# E(Beta(alpha,beta)) = alpha/(alpha+beta)
# Var(Beta(alpha,beta)) = alpha*beta/((alpha+beta)^2*(alpha+beta+1))
# Sigma(Beta(alpha,beta)) = sqrt(alpha*beta/((alpha+beta)^2*(alpha+beta+1)))
Beta <- function(alpha, beta) {
  E <- alpha/(alpha+beta)
  Var <- alpha*beta/((alpha+beta)^2*(alpha+beta+1))
  Sigma <- sqrt(alpha*beta/((alpha+beta)^2*(alpha+beta+1)))
  return(list(E = E, Var = Var, Sigma = Sigma))
}
