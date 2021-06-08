#Code for 1.2

## The following code chunk downloading the data is taken from Johan Emil and Jonathans GitHub.
#Libraries

library(quantmod)

## Scraping S&P 500 data from Yahoo! Finance. The ticker is GSPC
environment_sp500 <- new.env()
suppressWarnings(getSymbols(c("^GSPC"), env = environment_sp500, src = "yahoo",from = as.Date("1960-01-01"),
                            to = as.Date("2021-02-01")))

sp500 <- environment_sp500$GSPC

## Placing data into a more familiar data structure


df_sp500 <- data.frame(date = index(sp500), coredata(sp500))


# kommentar

#Me again
#We will extract a vector of stock values and subtract the shifted one.
prices <- df_sp500$GSPC.Adjusted
plot(df_sp500$date, df_sp500$GSPC.Adjusted)

#It turns out that diff(a) does excatly what i want.
logprices <- log(prices)
logincriments <- diff(logprices)
hist(logincriments, breaks = 250, prob = TRUE)

#The dist'n looks a little pointy.
#There alsp seems to be an overoccurance of extreme values.
#Perhaps we have a t-distribution with a lower amount of degrees of freedom?
#It also seems that the positive incriments are more stable than the negative ones.
#The distribution has heavier tails than a gaussian.

logincriments_mean <- mean(logincriments)
sigma <- sd(logincriments)
mu <- logincriments_mean + (1/2)*sigma^2
x <- seq(from = -0.1, to = 0.1, by = 0.005)
y <- dnorm(x, mean = logincriments_mean - (1/2)*sigma^2, sd = sigma)
lines(x,y)
qqnorm(y = logincriments, mean = logincriments_mean, sd = sigma); qqline(logincriments)

#What are the prices for 2020?
prices_2020 <- prices[15103:15355]
logprices_2020 <- log(prices_2020)
logincriments_2020 <- diff(logprices_2020)

prices_before <- prices[1:15102]
logprices_before <- log(prices_before)
logincriments_before <- diff(logprices_before)
#Perhaps it is time to write a function that estimates the parameters in the model given a vector of prices.
#We should bear in mind that the parameters in the model specified in 1.4 doesn't neccesarily coincide with the standard parametrization of the log-normal dist'n.
black_scholes_fit <- function(x){
  prices <- x
  logprices <- log(prices)
  logincriments <- diff(logprices)
  logincriments_mean <- mean(logincriments)
  sigma <- sd(logincriments)
  mu <- logincriments_mean + (1/2)*sigma^2
  params <- c(mu, sigma)
  params
}
black_scholes_fit(prices)
black_scholes_fit(prices_2020)

t.test(x = logincriments_before, y = logincriments_2020, alternative = c("greater"))

recent_data <- df_sp500[(12500: 15000),]
recent_logincriments <- diff(log(recent_data$GSPC.Adjusted))
hist(recent_logincriments, breaks = 250, prob = T)
x <- seq(from = -0.08, to = 0.06, by = 0.0005)
y <- dnorm(x, mean = mean(recent_logincriments), sd = sd(recent_logincriments))
lines(x,y)
qqnorm(y = recent_logincriments, mean = mean(recent_logincriments), sd = sd(recent_logincriments)); qqline(recent_logincriments)
recent_data[1,]
recent_data[2501,]