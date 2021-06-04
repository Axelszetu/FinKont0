#Code for computing option prices in the BS model.
library(tidyverse)

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
