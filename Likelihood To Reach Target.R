library(RCurl)
# Read data from Github - Qty sold over 15 years and in millions
mydata <- read.csv(text = getURL("https://raw.githubusercontent.com/tristanga/MonteCarlo_ForecastRisk/master/TS.csv"))
# Create time serie
tseries <- ts(mydata$x, frequency = 12, start = c(2000, 1))
# Check time serie
start(tseries)
end(tseries)
frequency(tseries)
# Remove Q4
tseries_sub <- window(tseries, start=c(2000, 1), end=c(2015,9))
# Define your target
mytarget = 186.0000
# Calculat YTD (Q1-Q3)
actualYTD <- sum(window(tseries, start=c(2015, 1), end=c(2015,9)))
# Check the distribution of the time serie
hist(tseries_sub)
boxplot(tseries_sub)
# Create a data frame with time serie
tseries_df = as.data.frame(tseries_sub)
# Option A - Fit a standard distribution
library(fitdistrplus)
fit.norm <- fitdist(as.numeric(tseries_df$x), "norm")
fit.exp <- fitdist(as.numeric(tseries_df$x), "exp")
fit.weibull <- fitdist(as.numeric(tseries_df$x), "weibull")
fit.lnorm <- fitdist(as.numeric(tseries_df$x), "lnorm")
fit.gamma <- fitdist(as.numeric(tseries_df$x), "gamma")
fit.logistic <- fitdist(as.numeric(tseries_df$x), "logis")
fit.cauchy <- fitdist(as.numeric(tseries_df$x), "cauchy")
# compare Goodness-of-fit statistics
gofstat(list(fit.norm, fit.exp, fit.weibull,fit.lnorm,fit.gamma,fit.logistic,fit.cauchy),
        fitnames = c("fit.norm", "fit.exp", "fit.weibull","fit.lnorm","fit.gamma","fit.logistic","fit.cauchy"))
# Normal distribution has the best Goodness-of-fit statistics
option1 = fit.norm
plot(option1)
summary(option1)
# Option B - Fit a supplementary distribution
library(SuppDists)
parms<-JohnsonFit(as.numeric(tseries_df$x), moment="quant")
# plot the distribution
hist( as.numeric(tseries_df$x) , freq=FALSE)
plot(function(x)dJohnson(x,parms), 0, 20, add=TRUE, col="red")

# Generate samples for Option A
library(truncnorm)
fit.coef <- coef(fit.norm)
final_df1 <- as.data.frame(rtruncnorm(n=10^4, a=min(tseries_df$x), b=max(tseries_df$x), mean=fit.coef["mean"], sd=fit.coef["sd"]))
colnames(final_df1) <- 'Oct'
final_df1$Nov <- rtruncnorm(n=10^4, a=min(tseries_df$x), b=max(tseries_df$x), mean=fit.coef["mean"], sd=fit.coef["sd"])
final_df1$Dec <- rtruncnorm(n=10^4, a=min(tseries_df$x), b=max(tseries_df$x), mean=fit.coef["mean"], sd=fit.coef["sd"])
final_df1$Forecast <- actualYTD + final_df1$Oct + final_df1$Nov +final_df1$Dec
# Plot Option A forecasted samples
hist(final_df1$Forecast)
# Generate samples for Option B
option2 <- function(x)qJohnson(x,parms)
option2sample <- myfunction(runif(10000))
hist(option2sample)
boxplot(option2sample,as.numeric(tseries_df$x) )
final_df2 <- as.data.frame(myfunction(runif(10000)))
colnames(final_df2) <- 'Oct'
final_df2$Nov <- myfunction(runif(10000))
final_df2$Dec <- myfunction(runif(10000))
final_df2$Forecast <- actualYTD + final_df2$Oct + final_df2$Nov +final_df2$Dec
# Plot Option B forecasted samples
hist(final_df2$Forecast)

#Summary of Option A and Option B approaches - Probability to achieve target is less than 33%
boxplot(final_df$Forecast,final_df1$Forecast)
myproba1 <- sum( final_df1$Forecast >= 186 ) / 100
myproba2 <- sum( final_df2$Forecast >= 186 ) / 100
