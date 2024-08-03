degreeOfFreedom <- function(n) {
  # Calculate the degrees of freedom for the one sample
  df <- n - 1
  df
}

degreesOfFreedom <- function(n1,n2) {
  # Calculate the degrees of freedom for two samples
  df <- n1 + n2 - 2
  df
}

pooledVarianceEstimator <- function(v1,n1,v2,n2) {
  # Calculate the pooled variance
  w1 <- (n1-1)*(v1)
  w2 <- (n2-1)*(v2)
  df <- degreesOfFreedom(n1,n2)
  pv <- (w1+w2)/df
  pv
}

meanSE <- function(s,n) {
  # Calculate the mean Margin of Error
  s/sqrt(n)
}

proportionSE <- function(p,n) {
  # Calculate the population Margin of Error
  sqrt(p*(1-p)/n)
}

meanDiffSE <- function(s1,n1,s2,n2) {
  # Calculate the Margin of Error for the difference of two means
  sqrt((s1^2/n1)+(s2^2/n2))
}

pooledMeanDiffSE <- function(n1,n2) {
  # Calculate the Margin of Error for the difference of two means
  sqrt((1/n1)+(1/n2))
}

proportionDiffSE <- function(p1,n1,p2,n2) {
  # Calculate the Margin of Error for the difference of two proportions
  sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
}

stddevSE <- function(s,n) {
  # Calculate the Margin of Error for a standard deviation
  s/sqrt(2*(n-1))
}

varianceSE <- function(s,n) {
  # Calculate the Margin of Error for a variance
  sqrt(2*s^2/(n-1))
}

rejectionRegionZTest <- function(a) {
  # Calculate the rejection region for a z-test
  c(qnorm(a/2),qnorm(1 - a/2))
}

rejectionRegionTTest <- function(a,df) {
  # Calculate the rejection region for a t-test
  c(qt(a/2,df),qt(1 - a/2,df))
}

rejectionRegionFTest <- function(a,df1,df2) {
  # Calculate the rejection region for an F-test
  c(qf(a/2,df1,df2),qf(1 - a/2,df1,df2))
}

rejectionRegionCTest <- function(a,df) {
  # Calculate the rejection region for a chi-squared test
  c(qchisq(a/2,df),qchisq(1 - a/2,df))
}
