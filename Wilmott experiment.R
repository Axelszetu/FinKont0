wilmott_experiment_maker <- function(S_0 = 1,
                                     Strike = 1,
                                     r = 0.03,
                                     mu = 0.07,
                                     sigma = 0.2,
                                     implied_sigma = 0.15,
                                     hedge_sigma = 0.2,
                                     dt = 1/255,
                                     M = 1,
                                     n = 1000){
  m <- M/dt
  PnL <- rep(NA, n)
  
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
    #we start by selling an option, so our initial balance is C(t)
    #browser()
    balance <- 0
    
    deltas <- pnorm(d1_evaluator(S = S, sigma = hedge_sigma, time = 1 - dates))
    deltas1 <- deltas[2:m]
    deltas2 <- deltas[3:(m+1)]
    
    option_payoff <- 0
    if (S[m+1] > Strike){
      option_payoff <- S[m+1] - Strike
    }
    
    hedging_balance <- deltas[1]*S[1]*discount_vector[1] + sum(discount_vector[2:m]*S[2:m]*(deltas1 - deltas2) + deltas[m]*S[m+1])
    balance <- hedging_balance + option_payoff - option_pricer_analytical(S = S[1], K = Strike, time = M, r = r, sigma = implied_sigma)*discount_vector[1]
    PnL[j] <- balance
  }
  
  PnL
}

#Construction a signel trial of the hedge experiment
M <- 1
dt <- 1/255
mu <- 0.07
m = M/dt
dW <- rnorm(m, mean = 0, sd = 1) * sqrt(dt)
sigma <- 0.2
implied_sigma <- 0.15
hedge_sigma <- 0.2
r <- 0.03

S <- rep(NA, m+1)
S[1] <- 1
for (i in (2:(m+1))){
  S[i] <- S[i-1] + mu*S[i-1]*dt + sigma*S[i-1]*dW[i-1]
}


