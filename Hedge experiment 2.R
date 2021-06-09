hedge_experiment_maker2 <- function(S_0 = 1, Strike = 1, r = 0.03, mu = 0.07, sigma = 0.2, dt = 1/255, M = 1, n = 1000, fake_sigma = 0){
  
  end_balance <- rep(NA, n)
  S_T <- rep(NA, n)
  true_sigma <- sigma
  m <- M/dt
  
  for (j in (1:n)){
    sigma <- true_sigma
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
    
    balance <- option_pricer_analytical(S = S_0, K = Strike, time = 1, r = r, sigma = sigma)*discount_vector[1]
    #balance <- 0
    if (fake_sigma > 0){
      sigma <- fake_sigma
    }
    deltas <- pnorm(d1_evaluator(S = S, sigma = sigma, time = 1 - dates))
    deltas <- deltas[1:m]
    S1 <- S[1:m]
    S2 <- S[2:(m+1)]
    balance <- balance - sum(S1*deltas*discount_vector1) + sum(S2*deltas*discount_vector2)
    #if(S[m+1] > Strike){
    #  balance <- balance - S[m+1] + Strike
    #}
    
    end_balance[j] <- balance
    S_T[j] = S[m+1]
  }
  
  factor <- as.factor(1/dt)
  factor2 <- as.factor(Strike)
  factor3 <- as.factor(fake_sigma)
  df <- data.frame(end_balance, S_T, steps = factor, Strike = factor2, fake_sigma = factor3)
  df
}

experiment_data <- hedge_experiment_maker2(n = 1000)
experiment_data_low <- hedge_experiment_maker2(fake_sigma = 0.05, n = 1000)
experiment_data_high <- hedge_experiment_maker2(fake_sigma = 0.4, n = 1000)
comparison_data <- rbind(experiment_data, experiment_data_low, experiment_data_high)
experiment_plot <- ggplot(comparison_data, aes(x = S_T, y = end_balance, colour = fake_sigma)) + geom_point()
experiment_plot
x1 <- seq(from = 0.5, to = 1, by = 0.001)
x2 <- seq(from = 0, to = 2, by = 0.001)
y1 <- rep(0, 501)
y2 <- x2
x2 <- x2 + 1
x <- c(x1, x2)
y <- c(y1, y2)
lines(x,y)

plot(experiment_data$S_T, experiment_data$end_balance)
lines(x,y)

plot(experiment_data_low$S_T, experiment_data_low$end_balance)
lines(x,y)

plot(experiment_data_high$S_T, experiment_data_high$end_balance)
lines(x,y)