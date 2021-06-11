#MC estimation of lognormal distribution
n <- 10000
normals <- rnorm(n, mean = 0.07, sd = 0.2)
lognormals <- exp(normals)
cumulative_means <- cumsum(lognormals)*(1/(1:n))
mean(lognormals)
plot_data <- data.frame(means = cumulative_means, n = (1:n))
MC_plot <- ggplot(plot_data, aes(x = n, y = cumulative_means)) + geom_line() + geom_hline(yintercept = exp(0.07 + (1/2) * 0.2^2), color = "red")
MC_plot