hedge_experiment_maker <- function(S_0 = 1, Strike = 1, r = 0.03, mu = 0.07, sigma = 0.2, dt = 1/255, M = 1, n = 1000){
  
  outcomes <- rep(NA, n)
  
  m <- M/dt
  
  for (j in (1:n)){
    dW <- rnorm(m, mean = 0, sd = 1) * sqrt(dt)
    
    S <- rep(NA, m+1)
    S[1] <- 1
    for (i in (2:(m+1))){
      S[i] <- S[i-1] + mu*S[i-1]*dt + sigma*S[i-1]*dW[i-1]
    }
    #at this point our path has been made
    
    #we can compute a before starting accounting
    a <- numeric(length = m)
    for (i in (1:m-1)){
      a[i] <- d1_evaluator(S = S[i], K = Strike, time = 1 - i/255, r = r, sigma = sigma)
    }
    #now we wanna do accounnting to keep track of portfolio balance
    #we start by selling an option, so our initial balance is C(t)
    balance <- option_pricer_analytical(S = S_0, K = Strike, time = 1, r = r, sigma = sigma)
    
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