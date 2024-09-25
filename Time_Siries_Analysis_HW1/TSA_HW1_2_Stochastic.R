library(ggplot2)

data <- data.frame(
  x = seq(1,47,1),
  y = 0
)

i=1
while (i <= 47) {
  phi = runif(1)
  data$y[i] <- cos(2*pi*(data$x[i]/12+phi))
  i <- i + 1
}

tt = expression(Y[t] == cos(2*pi(frac(t,12)+phi)))

ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "#037ffc", size = 1.5) + 
  geom_line(color = "#037ffc") +
  labs(x = "Time", y = "Y_t", title = tt) +
  theme_minimal()

