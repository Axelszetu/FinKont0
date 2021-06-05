#In this script we load and analyze option data on S&P 500
library(quantmod)
library(tidyverse)


#euro_option_data <-  getOptionChain(Symbols = "^SPX", Exp = c("2021", "2022"),
#                                    data_source = "yahoo")

load("euro_option_data.RData")

options_aug31 <- euro_option_data[["Sep.30.2021"]][["calls"]]
traded <- options_aug31[["IV"]] > 0.01
traded_options_aug31 <- options_aug31[traded, ]

#Data was retrieced on 04/06/2021
#Adjusted close price was 4229.89

S <- 4229.89

traded_options_aug31$logmoneyness <- log(S/traded_options_aug31$Strike)
volatility_smile <- ggplot(traded_options_aug31, aes(x = logmoneyness, y = IV)) + geom_point()

traded_options_aug31$model_price <- option_pricer_analytical(S = S, K = traded_options_aug31$Strike, time = 0.25, r = 0.0002, sigma = 0.164302)

plot_data_emipirical <- data.frame(Strike = traded_options_aug31$Strike, Price = traded_options_aug31$Last, Label = "Empirical")
plot_data_theoretical <- data.frame(Strike = traded_options_aug31$Strike, Price =  traded_options_aug31$model_price, Label = "Theoretical")
graph_data <- rbind(plot_data_emipirical, plot_data_theoretical)
prices_plot <- ggplot(graph_data, aes(x = Strike, y = Price, color = Label)) + geom_point() + geom_vline(aes(xintercept = 4229.89))
prices_plot

traded_options_aug31[1,]

goal_function_maker <- function(K, L){
  goal_function <- function(sigma){
    d1 <- 1/(sigma*sqrt(0.25)) * (log(4229.89/K) + (0.0002 + ((sigma^2)/2)*(0.25)))
    d2 <- d1 - sigma*sqrt(0.25)
    price <- 4229.89*pnorm(d1) - exp(-0.0002*0.25)*K*pnorm(d2)
    out <- abs(price - L)
    out
  }
  goal_function
}

test_function <- goal_function_maker(K = 4220, L = 116.80)
optimize(test_function, interval = c(0,1), maximum = F)

my_IV <- numeric(length = 209)
for (i in (1:209)){
  my_IV[i] <- optimize(goal_function_maker(traded_options_aug31$Strike[i], traded_options_aug31$Ask[i]), interval = c(0,1), maximum = F)$minimum
}

IV_comparison_Yahoo <- data.frame(Strike = traded_options_aug31$Strike, IV = traded_options_aug31$IV, Label = "Yahoo")
IV_comparison_my <- data.frame(Strike = traded_options_aug31$Strike, IV = my_IV, Label = "my_IV")
IV_comparison_data <- rbind(IV_comparison_Yahoo, IV_comparison_my)
IV_comparison_plot <- ggplot(IV_comparison_data, aes(x = Strike, y = IV, color = Label)) + geom_point() + geom_vline(aes(xintercept = 4229.89)) + geom_hline(aes(yintercept = 0.164302), colour = "purple")
IV_comparison_plot