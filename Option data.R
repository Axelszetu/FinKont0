#In this script we load and analyze option data on S&P 500
library(quantmod)
library(tidyverse)


#euro_option_data <-  getOptionChain(Symbols = "^SPX", Exp = c("2021", "2022"),
#                                    data_source = "yahoo")

load("euro_option_data.RData")