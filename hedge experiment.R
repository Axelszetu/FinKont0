hedge_experiment_maker <- function(S_0 = 1, Strike = 1, r = 0.03, mu = 0.07, sigma = 0.2, dt = 1/255, M = 1, n = 1000, fake_sigma = 0){
  
  end_balance <- rep(NA, n)
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
    
    end_balance[j] <- balance
  }
  
  factor <- as.factor(1/dt)
  factor2 <- as.factor(Strike)
  factor3 <- as.factor(fake_sigma)
  df <- data.frame(end_balance, steps = factor, Strike = factor2, fake_sigma = factor3)
  df
}

df1 <- hedge_experiment_maker(dt = 1)
df2 <- hedge_experiment_maker(dt = 1/6)
df3 <- hedge_experiment_maker(dt = 1/36)
df4 <- hedge_experiment_maker(dt = 1/216)
df5 <- hedge_experiment_maker(dt = 1/(6^4))
df6 <- hedge_experiment_maker(dt = 1/(6^5))
plot_data <- rbind(df1, df2, df3, df4, df5)
hedge_experiment_plot <- ggplot(plot_data, aes(x = steps, y = end_balance, fill = steps)) + geom_violin() + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", colour = "black")
hedge_experiment_plot
dfs <- list(df1, df2, df3, df4, df5, df6)
deviations <- numeric()
for (i in (1:5)){
  deviations[i] <- sd(dfs[[i]]$end_balance)
}
deviations
log_deviations <- log(deviations)
x <- 6^(0:4)
logx <-  log(x)
deviations_plot_data <- data.frame(log_deviations = log_deviations, logx = logx)
ggplot(deviations_plot_data, aes(x = logx, y = log_deviations)) + geom_point()

means <- numeric()
for (i in (1:5)){
  means[i] <- mean(dfs[[i]]$end_balance)
}
means

strikes <- c(0.1, 0.5, 1, 5, 10, 100)
strike_comparison <- lapply(strikes, hedge_experiment_maker, dt = 1/216, S_0 = 1)
strike_comparison_df <- do.call(rbind, strike_comparison)
strike_comparison_plot <-  ggplot(strike_comparison_df, aes(x = Strike, y = end_balance, fill = Strike)) + geom_violin() + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", colour = "black")
strike_comparison_plot

volatilities <- c(0.01, 0.05, 0.2, 0.4, 0.8)
volatility_comparison <- lapply(volatilities, hedge_experiment_maker, S_0 = 1, Strike = 1, r = 0.03, mu = 0.07, sigma = 0.2, dt = 1/255, M = 1, n = 1000)
volatility_comparison_df <- do.call(rbind, volatility_comparison)
volatility_comparison_plot <- ggplot(volatility_comparison_df, aes(x = fake_sigma, y = end_balance, fill = fake_sigma)) + geom_violin() + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", colour = "black")
volatility_comparison_plot
different_sigma_means <- numeric()
for (i in (1:5)){
  different_sigma_means[i] <- mean(abs(volatility_comparison[[i]]$end_balance))
}
different_sigma_means

low_volatility_mesh <- c(1, 1/6, 1/36, 1/216, 1/(6^4))
low_volatility_comparison <- lapply(low_volatility_mesh, hedge_experiment_maker, S_0 = 1, Strike = 1, r = 0.03, mu = 0.07, sigma = 0.2, M = 1, n = 1000, fake_sigma = 0.05)
low_volatility_comparison_df <- do.call(rbind, low_volatility_comparison)
low_volatility_comparison_plot <- ggplot(low_volatility_comparison_df, aes(x = steps, y = end_balance, fill = steps)) + geom_violin() + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", colour = "black")
low_volatility_comparison_plot
low_volatility_means <- numeric()
for (i in (1:5)){
  low_volatility_means[i] <- mean(abs(low_volatility_comparison[[i]]$end_balance))
}
low_volatility_means

deviations_low <- numeric()
for (i in (1:5)){
  deviations_low[i] <- sd(low_volatility_comparison[[i]]$end_balance)
}
deviations_low
log_deviations_low <- log(deviations_low)
x <- 6^(0:4)
logx <-  log(x)
deviations_plot_data <- data.frame(log_deviations_low = log_deviations_low, logx = logx)
ggplot(deviations_plot_data, aes(x = logx, y = log_deviations_low)) + geom_point(color = "black")

#the following will reuse the code and names for low volatility while actually producing for high
low_volatility_mesh <- c(1, 1/6, 1/36, 1/216, 1/(6^4))
low_volatility_comparison <- lapply(low_volatility_mesh, hedge_experiment_maker, S_0 = 1, Strike = 1, r = 0.03, mu = 0.07, sigma = 0.2, M = 1, n = 1000, fake_sigma = 0.4)
low_volatility_comparison_df <- do.call(rbind, low_volatility_comparison)
low_volatility_comparison_plot <- ggplot(low_volatility_comparison_df, aes(x = steps, y = end_balance, fill = steps)) + geom_violin() + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", colour = "black")
low_volatility_comparison_plot
low_volatility_means <- numeric()
for (i in (1:5)){
  low_volatility_means[i] <- mean(low_volatility_comparison[[i]]$end_balance)
}
low_volatility_means

deviations_high <- numeric()
for (i in (1:5)){
  deviations_high[i] <- sd(low_volatility_comparison[[i]]$end_balance)
}
deviations_high
log_deviations_high <- log(deviations_high)
deviations_plot_data_high <- data.frame(log_deviations_high = log_deviations_high, logx = logx)
ggplot(deviations_plot_data, aes(x = logx, y = log_deviations_high)) + geom_point(color = "black")