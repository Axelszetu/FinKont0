Wilmott_experiment_maker <- function(
    S = 1,
    K = 1,
    r = 0.03,
    mu = 0.07,
    sigma = 0.2,
    implied_sigma = 0.15,
    hedge_sigma = 0.2,
    dt = 1/255,
    M = 1,
    n = 1000){
  m <- M/dt
  
  for (j in (1:n)){
    dW <- rnorm(m, mean = 0, sd = 1) * sqrt(dt)
    
    S <- rep(NA, m+1)
    S[1] <- S_0
    for (i in (2:(m+1))){
      S[i] <- S[i-1] + mu*S[i-1]*dt + sigma*S[i-1]*dW[i-1]
    }
    #at this point our path has been made
    
    #now we wanna do accounnting to keep track of portfolio balance
    #we will need to discount along the way so we make a discount vector
    dates <- seq(from = 0, to = M, length.out = (m+1))
    discount_vector <- exp(r*(M-dates))
    discount_vector1 <- discount_vector[1:m]
    discount_vector2 <- discount_vector[2:(m+1)]
    #we start by selling an option, so our initial balance is C(t)
    #browser()
    
    balance <- option_pricer_analytical(S = S_0, K = Strike, time = 1, r = r, sigma = sigma)
    if (fake_sigma > 0){
      sigma <- fake_sigma
    }
    deltas <- pnorm(d1_evaluator(S = S, sigma = sigma, time = 1 - dates))
    deltas <- deltas[1:m]
    S1 <- S[1:m]
    S2 <- S[2:(m+1)]
    balance <- balance - sum(S1*deltas*discount_vector1) + sum(S2*deltas*discount_vector2)
    if(S[m+1] > Strike){
      balance <- balance - S[m+1] + Strike
    }
    
    
  }
  
  df <- data.frame(end_balance, steps = factor, Strike = factor2, fake_sigma = factor3)
  df
}
}