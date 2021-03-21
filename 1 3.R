#1.3 - Monte-Carlo Simulation

#d) we wisha plot of 10 paths for different dt
plot_data <- list()
dt <- c(10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000)
dt <- 1/dt

for (i in (1:10)){
  plot_data[[i]] <- make_path(dt = dt[i])
}

bundled_plot_data <- plot_data[[1]]
for (i in (2:10)){
  bundled_plot_data <- rbind(bundled_plot_data, plot_data[[i]])
}

ggplot(bundled_plot_data, aes(x = x, y = S, colour = dt)) +
  geom_line()

