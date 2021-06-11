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

#On the topic of discretization error
#We wish to illustrate convergence of the MC-algorithm, but to differenc values of S

plot_data <- list()
dt <- c(5, 50, 500, 5000, 50000)
dt <- 1/dt


for (i in (1:length(dt))){
  plot_data[[i]] <- MC_convergence_tracker(dt = dt[i], n = 1500)
}

bundled_plot_data <- plot_data[[1]]
for (i in (2:length(dt))){
  bundled_plot_data <- rbind(bundled_plot_data, plot_data[[i]])
}

ggplot(bundled_plot_data, aes(x = x, y = means, colour = dt)) +
  geom_line() + geom_hline(yintercept = exp(0.07 + (1/2) * 0.2^2)) + coord_cartesian(ylim = c(1.04, 1.1))