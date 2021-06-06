#Functions
library(tidyverse)

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

MC_stock <- function(mu = 0.07, sigma = 0.2, dt = 1/255, M = 1, n = 1000, antithetic_variates = TRUE){
  
  outcomes <- rep(NA, n)
  if (antithetic_variates == TRUE){
    outcomes <- rep(NA, 2*n)
  }
  
  m <- M/dt
  
  for (j in (1:n)){
    dW <- rnorm(m, mean = 0, sd = 1) * sqrt(dt)
    
    S <- rep(NA, m+1)
    S[1] <- 1
    for (i in (2:(m+1))){
      S[i] <- S[i-1] + mu*S[i-1]*dt + sigma*S[i-1]*dW[i-1]
    }
    
    outcomes[j] <- S[m+1]
    
    if(antithetic_variates == TRUE){
      Sant <- rep(NA, m+1)
      Sant[1] <- 1
      for (i in (2:(m+1))){
        Sant[i] <- Sant[i-1] + mu*Sant[i-1]*dt - sigma*Sant[i-1]*dW[i-1]
      }
      
      outcomes[n+j] <- Sant[m+1]
    }
    
  }
  c(mean(outcomes), sd(outcomes))
}

make_path <- function(mu = 0.07, sigma = 0.2, dt = 1/255, M = 1){
  m <- M/dt
  dW <- rnorm(m, mean = 0, sd = 1) * sqrt(dt)
  S <- rep(NA, m+1)
  S[1] <- 1
  for (i in (2:(m+1))){
    S[i] <- S[i-1] + mu*S[i-1]*dt + sigma*S[i-1]*dW[i-1]
  }
  x <- seq(from = 0, to = M, by = dt)
  factor <- as.factor(dt)
  path <- data.frame(x, S, dt = factor)
  path
}

MC_convergence_tracker <- function(mu = 0.07, sigma = 0.2, dt = 1/255, M = 1, n = 1000){
  
  outcomes <- rep(NA, n)
  
  m <- M/dt
  
  for (j in (1:n)){
    dW <- rnorm(m, mean = 0, sd = 1) * sqrt(dt)
    
    S <- rep(NA, m+1)
    S[1] <- 1
    for (i in (2:(m+1))){
      S[i] <- S[i-1] + mu*S[i-1]*dt + sigma*S[i-1]*dW[i-1]
    }
    
    outcomes[j] <- S[m+1]
    
  }
  
  
  means <- rep(NA,n)
  for (i in (1:n)){
    means[i] <- mean(outcomes[1:i])
  }
  
  x <- c(1:n)
  factor <- as.factor(dt)
  df <- data.frame(x = x, means, dt = factor)
  df
}

option_pricer_analytical <- function(S = 1, K = 1, time = 1, r = 0.03, sigma = 0.2){
  d1 <- 1/(sigma*sqrt(time)) * (log(S/K) + (r + ((sigma^2)/2)*(time)))
  d2 <- d1 - sigma*sqrt(time)
  price <- S*pnorm(d1) - exp(-r*time)*K*pnorm(d2)
  price
}

option_pricer_MC <- function(n = 10000, S = 1, K = 1, time = 1, r = 0.03, sigma = 0.2, withplot = FALSE){
  discount_factor <- exp(-r*time)
  prices <- rlnorm(n = n, meanlog = (r - (sigma^2)/2)*time, sdlog = sigma)
  payoffs <- prices - K
  inthemoney <- (payoffs >= 0)
  payoffs <- payoffs*inthemoney
  payoffs <- payoffs*discount_factor
  estimates <- c(mean(payoffs), sd(payoffs)/sqrt(n))
  out <- list()
  out$estimates <- estimates
  if (withplot == TRUE){
    d1 <- 1/(sigma*sqrt(time)) * (log(S/K) + (r + ((sigma^2)/2)*(time)))
    d2 <- d1 - sigma*sqrt(time)
    BS_price <- S*pnorm(d1) - exp(-r*time)*K*pnorm(d2)
    
    estimate_n <- numeric(length = n)
    var_n <- numeric(length = n)
    sd_n <- numeric(length = n)
    for (i in (1:n)){
      estimate_n[i] <- mean(payoffs[1:i])
      var_n[i] <- mean((payoffs[1:i] - estimate_n[i])^2)/n
      sd_n[i] <- sqrt(var_n[i])
    }
    
    running_estimates <- data.frame(estimate_n, sd_n)
    out$plot <- ggplot(running_estimates, aes(x = c(1:n), y = estimate_n)) +
      geom_ribbon(mapping = aes(
        ymin = estimate_n - 1.96*sd_n,
        ymax = estimate_n + 1.96*sd_n
      ), fill = "gray"
      ) + geom_point() + geom_line() +
      geom_hline(yintercept = BS_price, color = "red")
  }
  out
}

d1_evaluator <- function(S = 1, K = 1, time = 1, r = 0.03, sigma = 0.2){
  d1 <- 1/(sigma*sqrt(time)) * (log(S/K) + (r + ((sigma^2)/2)*(time)))
  d1
}