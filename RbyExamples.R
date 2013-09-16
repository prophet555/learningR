# Learn R by doing 
# http://www.mayin.org/ajayshah/KB/R/html/b5.html

##############b3.R Prices and Returns
####################################
ranrw <- function(mu, sigma, p0=100, T=100) {
  cumprod(c(p0,1 + (rnorm(n=T, mean=mu, sd=sigma)/100)))
}
prices2returns <- function(x) {
  100*diff(log(x))     #use log returns for compounded
}
returns2prices <- function(r, p0=100) {
  c(p0, p0 * exp(cumsum(r/100)))
}
cat("Simulate 25 points in random walk starting at 1500 --\n")
p <- ranrw(0.05, 1.4, p0=1500, T=25)
print(p)

cat("Convert to returns --\n")
r <- prices2returns(p)
print(r)

cat("Go back from returns to prices --\n")
goback <- returns2prices(r, 1500)
print(goback)

# things to do with returns
summary(r)
sd(r)
plot(density(r))
acf(r)
ar(r)
Box.test(r, lag=2, type="Ljung")
library(tseries)
runs.test(factor(sign(r)))
bds.test(r)

###### prices series and what can happen, click in the graph window to draw new graphs
visualisation <- function(p0, s, mu, labelstring) {
  N <- 10000
  x <- (1:(N+1))/250                        # Unit of years
  while (1) {
    plot(x, ranrw(mu, s, p0, N), ylab="Level", log="y",
         type="l", col="red", xlab="Time (years)",
         main=paste("40 years of a process much like", labelstring))
    grid()
    z=locator(1)
  }
}
visualisation(2600, 1.4, 13/250, "Nifty")

# The numerical values here are used to think about what the INR/USD
# exchange rate would have looked like if it started from 31.37, had
# a mean depreciation of 5% per year, and had the daily vol of a floating
# exchange rate like EUR/USD.
visualisation(31.37, 0.7, 5/365, "INR/USD (NOT!) with daily sigma=0.7")
# This is of course not like the INR/USD series in the real world -
# which is neither a random walk nor does it have a vol of 0.7% a day.

# The numerical values here are used to think about what the USD/EUR
# exchange rate, starting with 1, having no drift, and having the observed
# daily vol of 0.7. (This is about right).
visualisation(1, 0.7, 0, "USD/EUR with no drift")

########
cat("\n\nExample 3: Making the NPV of a bond--\n")

C = rep(100, 6)
nsz(14.084, -3.4107, 0.0015, 1.832, timepoints)