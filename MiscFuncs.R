options(scipen=100,digits=6) #this is to avoid scientific notation

degreesOfFreedom <- function(n) {
  # Calculate the degrees of freedom for one or more samples
  if (length(n) <= 0)
    stop("n must have at least one element")
  if (!is.numeric(n))
    stop("n must be a numeric vector")
  
  sum(n) - length(n)
}

pooledVarianceEstimator <- function(v1,n1,v2,n2) {
  # Calculate the pooled variance
  w1 <- (n1-1)*(v1)
  w2 <- (n2-1)*(v2)
  df <- degreesOfFreedom(c(n1,n2))
  (w1+w2)/df
}

meanSE <- function(s,n) {
  # Calculate the mean Standard Error
  s/sqrt(n)
}

meanDiffSE <- function(s1,n1,s2,n2) {
  # Calculate the Standard Error for the difference of two means
  sqrt((s1^2/n1)+(s2^2/n2))
}

pooledMeanDiffSE <- function(s1,n1,s2,n2) {
  # Calculate the Standard Error for the difference of two pooled means
  pooledVarianceEstimator(s1^2,n1,s2^2,n1)*sqrt((1/n1)+(1/n2))
}

proportionSE <- function(p,n) {
  # Calculate the population Standard Error
  sqrt(p*(1-p)/n)
}

proportionDiffSE <- function(p1,n1,p2,n2) {
  # Calculate the Standard Error for the difference of two proportions
  sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
}

stddevSE <- function(s,n) {
  # Calculate the Standard Error for a standard deviation
  s/sqrt(2*(n-1))
}

varianceSE <- function(s,n) {
  # Calculate the Standard Error for a variance
  sqrt(2*s^2/(n-1))
}

rejectionRegionZ <- function(a) {
  # Calculate the rejection region for a z-test
  c(qnorm(a/2),qnorm(1 - a/2))
}

rejectionRegionT <- function(a,df) {
  # Calculate the rejection region for a t-test
  c(qt(a/2,df),qt(1 - a/2,df))
}

rejectionRegionC <- function(a,df) {
  # Calculate the rejection region for a chi-squared test
  c(qchisq(a/2,df),qchisq(1 - a/2,df))
}

rejectionRegionF <- function(a,df1,df2) {
  # Calculate the rejection region for an F-test
  c(qf(a/2,df1,df2),qf(1 - a/2,df1,df2))
}

pValueZ <- function(zstat, twotailed = FALSE) {
  # Calculate the P-Value from the Z-Statistic
  # If the Z-Statistic is greater than or equal to 0, the P-Value is 1 - the area to the left of the Z-Statistic
  if (zstat >= 0)
    pv <- 1 - pnorm(zstat)
  # If the Z-Statistic is less than 0, the P-Value is the area to the left of the Z-Statistic
  if (zstat < 0)
    pv <- pnorm(zstat)
  # If the test is two-tailed, the P-Value is doubled
  if (twotailed == TRUE)
    pv <- 2*(1 - pnorm(abs(zstat)))
  # Return the P-Value
  pv
}

pValueT <- function(tstat, df, twotailed = FALSE) {
  # Calculate the P-Value from the T-Statistic
  # If the T-Statistic is greater than or equal to 0, the P-Value is 1 - the area to the left of the T-Statistic
  if (tstat >= 0)
    pv <- pt(-tstat, df)
  # If the T-Statistic is less than 0, the P-Value is the area to the left of the T-Statistic
  if (tstat < 0)
    pv <- pt(tstat, df)
  # If the test is two-tailed, the P-Value is doubled
  if (twotailed == TRUE)
    pv <- 2*pt(abs(tstat), df)
  # Return the P-Value
  pv
}

pValueC <- function(cstat, df, twotailed = FALSE) {
  # Calculate the P-Value from the Chi-Squared Statistic
  # If the Chi-Squared Statistic is greater than or equal to 0, the P-Value is 1 - the area to the left of the Chi-Squared Statistic
  if (cstat >= 0)
    pv <- 1 - pchisq(cstat, df)
  # If the Chi-Squared Statistic is less than 0, the P-Value is the area to the left of the Chi-Squared Statistic
  if (cstat < 0)
    pv <- pchisq(cstat, df)
  # If the test is two-tailed, the P-Value is doubled
  if (twotailed == TRUE)
    pv <- 2*pchisq(abs(cstat), df)
  # Return the P-Value
  pv
}

pValueF <- function(fstat, df1, df2, twotailed = FALSE) {
  # Calculate the P-Value from the F-Statistic
  # If the F-Statistic is greater than or equal to 0, the P-Value is 1 - the area to the left of the F-Statistic
  if (fstat >= 0)
    pv <- 1 - pf(fstat, df1, df2)
  # If the F-Statistic is less than 0, the P-Value is the area to the left of the F-Statistic
  if (fstat < 0)
    pv <- pf(fstat, df1, df2)
  # If the test is two-tailed, the P-Value is doubled
  if (twotailed == TRUE)
    pv <- 2*pf(abs(fstat), df1, df2)
  # Return the P-Value
  pv
}

confidenceIntervalZ <- function(m, me, ci = 0.95) {
  # Calculate the Confidence Interval for a Z-Statistic
  q <- ci + (1 - ci)/2
  z <- qnorm(q)
  c(m - z*me, m + z*me)
}


confidenceIntervalT <- function(m, me, df, ci = 0.95) {
  # Calculate the Confidence Interval for a T-Statistic
  q <- ci + (1 - ci)/2
  t <- qt(q, df)
  c(m - t*me, m + t*me)
}