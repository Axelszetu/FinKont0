#Functions
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

MC_stock <- function(mu = 0.07, sigma = 0.2, dt = 1/255, M = 1, n = 1000){
  outcomes <- rep(NA, n)
  ruins <- 0
  for (j in (1:n)){
    
    outcomes[j] <- S[m+1]
    #if (any(S < 0)){
    #  ruins <- ruins + 1
    }
  mean(outcomes)
  #ruins
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
