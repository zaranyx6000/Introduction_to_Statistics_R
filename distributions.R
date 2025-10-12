
# Normal ------------------------------------------------------------------

# Load the ggplot2 package
library(ggplot2)

# Set the mean and standard deviation
mean_value <- 0
std_dev <- 1

# Generate a range of x values
x <- seq(-3, 3, by = 0.01)

# Calculate the corresponding y values using the Normal distribution PDF
y <- dnorm(x, mean = mean_value, sd = std_dev)

# Create a data frame with x and y values
data <- data.frame(x = x, y = y)

# Create the ggplot object
plot <- ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1) +
  labs(x = "X", y = "Probability Density", title = "Normal Distribution") +
  theme_minimal() +
  geom_vline(xintercept = mean_value, color = "red", linetype = "dashed", size = 1)

# Display the plot
print(plot)

# Poisson -----------------------------------------------------------------

# Load the ggplot2 package
library(ggplot2)

# Set the parameter lambda (average rate)
lambda <- 5

# Generate a range of x values
x <- 0:20

# Calculate the corresponding y values using the Poisson PMF
y <- dpois(x, lambda)

# Create a data frame with x and y values
data <- data.frame(x = factor(x), y = y)

# Create the ggplot object
plot <- ggplot(data, aes(x = x, y = y)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Number of Events (X)", y = "Probability", 
       title = "Poisson Distribution", subtitle = expression(lambda == 5)) +
  theme_minimal()

# Display the plot
print(plot)

# Binomial ----------------------------------------------------------------

# Load the ggplot2 package
library(ggplot2)

# Set the parameters for the Binomial distribution
n <- 10    # Total number of trials
p <- 0.3   # Probability of success on each trial

# Generate a range of x values (number of successes)
x <- 0:n

# Calculate the corresponding y values using the Binomial PMF
y <- dbinom(x, size = n, prob = p)

# Create a data frame with x and y values
data <- data.frame(x = factor(x), y = y)

# Create the ggplot object
plot <- ggplot(data, aes(x = x, y = y)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Number of Successes (k)", y = "Probability", 
       title = "Binomial Distribution", 
       subtitle = paste("n =", n, ", p =", p)) +
  theme_minimal()

# Display the plot
print(plot)
