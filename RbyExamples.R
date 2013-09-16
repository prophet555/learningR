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

# Measure the effectiveness of the runs test when faced with an
# AR(1) process of length 100 with a coeff of 0.1
set.seed(101)
one.ts <- function() {arima.sim(list(order = c(1,0,0), ar = 0.1), n=100)}
table(replicate(1000, runs.test(factor(sign(one.ts())))$p.value < 0.05))
# We find that the runs test throws up a prob value of below 0.05
# for 91 out of 1000 experiments.
# Wow! :-)
# To understand this, you need to look up the man pages of:
#    set.seed, arima.sim, sign, factor, runs.test, replicate, table.
# e.g. say ?replicate

######## b5 NPV bond, Nelson yield curve
nsz <- function(b0, b1, b2, tau, t) {
  tmp = t/tau
  tmp2 = exp(-tmp)
  return(b0 + ((b1+b2)*(1-tmp2)/(tmp)) - (b2*tmp2))
}

timepoints <- c(0.01,1:5)
cat("\n\nExample 3: Making the NPV of a bond--\n")

C = rep(100, 6)
nsz(14.084, -3.4107, 0.0015, 1.832, timepoints)

##### Writing Functions ##########
powerful <- function(x) {
  return(list(x2=x*x, x3=x*x*x, x4=x*x*x*x));
}
print("Showing powers of 3 --"); print(powerful(3));

# In R, the last expression in a function is, by default, what is
# returned. So you could equally just say:
powerful <- function(x) {list(x2=x*x, x3=x*x*x, x4=x*x*x*x)}

# Goal: Utilise matrix notation
#       We use the problems of portfolio analysis as an example.

# Prices of 4 firms to play with, at weekly frequency (for calendar 2004) --
p <- structure(c(300.403, 294.604, 291.038, 283.805, 270.773, 275.506, 292.271, 292.837, 284.872, 295.037, 280.939, 259.574, 250.608, 268.84, 266.507, 263.94, 273.173, 238.609, 230.677, 192.847, 219.078, 201.846, 210.279, 193.281, 186.748, 197.314, 202.813, 204.08, 226.044, 242.442, 261.274, 269.173, 256.05, 259.75, 243, 250.3, 263.45, 279.5, 289.55, 291.95, 302.1, 284.4, 283.5, 287.8, 298.3, 307.6, 307.65, 311.9, 327.7, 318.1, 333.6, 358.9, 385.1, 53.6, 51.95, 47.65, 44.8, 44.85, 44.3, 47.1, 44.2, 41.8, 41.9, 41, 35.3, 33.35, 35.6, 34.55, 35.55, 40.05, 35, 34.85, 28.95, 31, 29.25, 29.05, 28.95, 24.95, 26.15, 28.35, 29.4, 32.55, 37.2, 39.85, 40.8, 38.2, 40.35, 37.55, 39.4, 39.8, 43.25, 44.75, 47.25, 49.6, 47.6, 46.35, 49.4, 49.5, 50.05, 50.5, 51.85, 56.35, 54.15, 58, 60.7, 62.7, 293.687, 292.746, 283.222, 286.63, 259.774, 259.257, 270.898, 250.625, 242.401, 248.1, 244.942, 239.384, 237.926, 224.886, 243.959, 270.998, 265.557, 257.508, 258.266, 257.574, 251.917, 250.583, 250.783, 246.6, 252.475, 266.625, 263.85, 249.925, 262.9, 264.975, 273.425, 275.575, 267.2, 282.25, 284.25, 290.75, 295.625, 296.25, 291.375, 302.225, 318.95, 324.825, 320.55, 328.75, 344.05, 345.925, 356.5, 368.275, 374.825, 373.525, 378.325, 378.6, 374.4, 1416.7, 1455.15, 1380.97, 1365.31, 1303.2, 1389.64, 1344.05, 1266.29, 1265.61, 1312.17, 1259.25, 1297.3, 1327.38, 1250, 1328.03, 1347.46, 1326.79, 1286.54, 1304.84, 1272.44, 1227.53, 1264.44, 1304.34, 1277.65, 1316.12, 1370.97, 1423.35, 1382.5, 1477.75, 1455.15, 1553.5, 1526.8, 1479.85, 1546.8, 1565.3, 1606.6, 1654.05, 1689.7, 1613.95, 1703.25, 1708.05, 1786.75, 1779.75, 1906.35, 1976.6, 2027.2, 2057.85, 2029.6, 2051.35, 2033.4, 2089.1, 2065.2, 2091.7), .Dim = c(53, 4), .Dimnames = list(NULL, c("TISCO", "SAIL", "Wipro", "Infosys")))
# Shift from prices to returns --
r <- 100*diff(log(p))

# Historical expected returns --
colMeans(r)
# Historical correlation matrix --
cor(r)
# Historical covariance matrix --
S <- cov(r)
S

# Historical portfolio variance for a stated portfolio of 20%,20%,30%,30% --
w <- c(.2, .2, .3, .3)
t(w) %*% S %*% w

# The portfolio optimisation function in tseries --
library(tseries)
optimised <- portfolio.optim(r)         # This uses the historical facts from r
optimised$pw                            # Weights
optimised$pm                            # Expected return using these weights
optimised$ps                            # Standard deviation of optimised port.

# Goal:
#       A stock is traded on 2 exchanges.
#       Price data is missing at random on both exchanges owing to non-trading.
#       We want to make a single price time-series utilising information
#          from both exchanges. I.e., missing data for exchange 1 will
#          be replaced by information for exchange 2 (if observed).

# Let's create some example data for the problem.
e1 <- runif(15)                         # Prices on exchange 1
e2 <- e1 + 0.05*rnorm(15)               # Prices on exchange 2.
cbind(e1, e2)
# Blow away 5 points from each at random.
e1[sample(1:15, 5)] <- NA
e2[sample(1:15, 5)] <- NA
cbind(e1, e2)

# Now how do we reconstruct a time-series that tries to utilise both?
combined <- e1                          # Do use the more liquid exchange here.
missing <- is.na(combined)
combined[missing] <- e2[missing]        # if it's also missing, I don't care.
cbind(e1, e2, combined)
