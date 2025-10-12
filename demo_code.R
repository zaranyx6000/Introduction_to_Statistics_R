
# Create an object in R ---------------------------------------------------

x <- 42

print(x)

x <- c(1, 2, 3, 4) # create a numerical vector

y <- c(TRUE, FALSE) # create a logical vector

# Removing objects in R ---------------------------------------------------

rm(x) # Removes the object x from the workspace

rm(list = ls()) # Removes all the objects from the workspace

# Load a CSV into R -------------------------------------------------------

library(readr)

starwars <- read_csv("data/starwars.csv")

# Select columns from your data -------------------------------------------

library(tidyverse)

starwars <- read_csv("data/starwars.csv")

df <- select(starwars, name, height) # Select columns name and height

# Filter rows from your data ----------------------------------------------

library(tidyverse)

starwars <- read_csv("data/starwars.csv")

df <- filter(starwars, height >= 175) # Filter rows according to condition

# Mutate your data --------------------------------------------------------

library(tidyverse)

starwars <- read_csv("data/starwars.csv")

df <- mutate(starwars, height_m = height/100) # Create a new column for height in metres

# Improve your workflow with {magrittr} -----------------------------------

library(tidyverse)

starwars <- read_csv("data/starwars.csv")

# Pipe each data manipulation operation to the next one
df <- starwars %>% 
    select(name, height, weight, species) %>% 
    filter(height >= 175) %>% 
    mutate(height_m = height/100)

# Build a simple plot -----------------------------------------------------

library(tidyverse)

starwars <- read_csv("data/starwars.csv")

# Create and print the graphical object
ggplot(data = starwars,
       mapping = aes(x = height))

ggplot(data = starwars,
       mapping = aes(x = height)) +
    geom_histogram(binwidth = 5) # specify binwidth

ggplot(data = starwars,
       mapping = aes(x = height, y = weight)) +
    geom_point()

starwars %>% 
    filter(weight < 200) %>% # Remove weight value point and pass to ggplot
    ggplot(mapping = aes(x = height, y = weight)) +
    geom_point()

# Five number summary -----------------------------------------------------

library(tidyverse)

wine <- read_csv("data/wine.csv")

summary(wine$alcohol)

min(wine$alcohol)

max(wine$alcohol)

# Measures of central tendency --------------------------------------------

library(tidyverse)

wine <- read_csv("data/wine.csv")

mean(wine$alcohol, na.rm = TRUE)

median(wine$alcohol, na.rm = TRUE)

wine %>% 
    group_by(quality) %>% # Group by the variable in question
    summarise(counts = n()) %>% # Count the number of observations
    filter(counts == max(counts)) %>% # Keep the maximum count
    pull(quality) # Pull the variable name

# Measures of spread or variability ---------------------------------------

library(tidyverse)

wine <- read_csv("data/wine.csv")

range(wine$alcohol, na.rm = TRUE) # range of variable

IQR(wine$alcohol, na.rm = TRUE) # interquartile range of variable

var(wine$alcohol, na.rm = TRUE) # variance of variable

sd(wine$alcohol, na.rm = TRUE) # standard deviation of variable

# Quantiles ---------------------------------------------------------------

library(tidyverse)

wine <- read_csv("data/wine.csv")

quantile(wine$alcohol, na.rm = TRUE) # Default: uses quartiles

quantile(wine$alcohol, probs = seq(from = 0, to = 1, by = 0.25), na.rm = TRUE) # same as above

quantile(wine$alcohol, probs = 0.25, na.rm = TRUE) # Lower Quartile

quantile(wine$alcohol, probs = 0.50, na.rm = TRUE) # Median

quantile(wine$alcohol, probs = 0.75, na.rm = TRUE) # Upper Quartile

# Univariate analysis - continuous variable -------------------------------

library(tidyverse)

wine <- read_csv("data/wine.csv")

# Histogram

mean_value <- mean(wine$fixed_acidity)

median_value <- median(wine$fixed_acidity)

wine %>%
    select(fixed_acidity) %>% 
    ggplot(mapping = aes(x = fixed_acidity)) + # specify x aesthetics
    # add histogram layer
    geom_histogram(binwidth = 0.5, colour = "black", fill = "grey") +
    # now add vertical line for mean
    geom_vline(xintercept = mean_value, lwd = 1, colour = "red") +
    # now add vertical line for median
    geom_vline(xintercept = median_value, lwd = 1, colour = "blue")

# Boxplot

wine %>%
    select(fixed_acidity) %>% 
    ggplot(mapping = aes(x = fixed_acidity)) + # specify x aesthetics
    # add boxplot layer with specification about the outliers
    geom_boxplot(outlier.colour = "red", outlier.shape = 4)

# Density plot

wine %>%
    select(fixed_acidity) %>% 
    ggplot(mapping = aes(x = fixed_acidity)) + # specify x aesthetics
    # add density layer with specification about the filling colour and transparency
    geom_density(colour = "black", fill = "grey", alpha = 0.5)

# Univariate analysis - categorical variable ------------------------------

library(tidyverse)

wine <- read_csv("data/wine.csv")

# Bar plot

wine %>% 
    select(quality) %>% 
    # specify x aesthetics
    ggplot(mapping = aes(x = quality)) + 
    # add bar plot layer
    geom_bar()

wine %>% 
    select(quality) %>% 
    # convert quality as a factor
    mutate(quality = factor(quality)) %>% 
    # specify x aesthetics
    ggplot(mapping = aes(x = quality)) + 
    # add bar plot layer with specification on how the bars are placed
    geom_bar(position = "dodge")

# Bivariate analysis - Continuous vs Continuous  --------------------------

library(tidyverse)

wine <- read_csv("data/wine.csv")

# Scatter plot

wine %>% 
    select(alcohol, density) %>% 
    # specify x and y aesthetics
    ggplot(mapping = aes(x = alcohol, y = density)) +
    # add points geometry layer
    geom_point()

# Bivariate analysis - Continuous vs Categorical  -------------------------

library(tidyverse)

wine <- read_csv("data/wine.csv")

# Boxplot

wine %>% 
    select(fixed_acidity, wine_type) %>% 
    mutate(wine_type = factor(wine_type)) %>%  # convert to factor
    # specify x and y aesthetics
    ggplot(mapping = aes(x = fixed_acidity, y = wine_type)) +
    # add boxplot layer
    geom_boxplot()

wine %>% 
    select(fixed_acidity, wine_type) %>% 
    mutate(wine_type = factor(wine_type)) %>%  # convert to factor
    # specify x and y aesthetics
    ggplot(mapping = aes(x = fixed_acidity, fill = wine_type)) +
    # add boxplot layer
    geom_boxplot()

# Bivariate analysis - Categorical vs Categorical  ------------------------

library(tidyverse)

wine <- read_csv("data/wine.csv")

# Bar plot

plot_data <- wine %>% 
    select(quality, wine_type) %>% 
    mutate(quality = factor(quality)) %>%  # convert to factor
    mutate(wine_type = factor(wine_type)) %>%  # convert to factor
    group_by(quality, wine_type) %>% 
    summarise(count = n()) %>% 
    ungroup()

plot_data %>%
    # specify x aesthetics
    ggplot(mapping = aes(x = quality, y = count, fill = wine_type)) +
    # add bar plot layer
    geom_col(position = "dodge")

# Correlation plot --------------------------------------------------------

library(ggcorrplot)
library(tidyverse)

wine <- read_csv("data/wine.csv")

plot_data <- wine %>% 
    select(where(is.numeric)) %>% # select only numerical columns in data
    cor()

ggcorrplot(plot_data, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 5)

# Random numbers from a Normal distribution -------------------------------

# Generate 10 random Normal distribution variables
# from a Normal distribution with mean = 10 and standard deviation = 2
random_normal <- rnorm(10, mean = 10, sd = 2)

print(random_normal)

set.seed(42) # set the random seed for reproducibility
random_normal <- rnorm(10, mean = 10, sd = 2)

print(random_normal)

# Random numbers from a Poisson distribution ------------------------------

# Generate 10 random Poisson distribution variables
# from a Poisson distribution with lambda = 5

set.seed(42) # set the random seed for reproducibility
random_poisson <- rpois(10, lambda = 5)

print(random_poisson)

# Random numbers from a Binomial distribution -----------------------------

# Generate 10 random Binomial distribution variables
# from a Binomial distribution with size = 10 and p = 0.5

set.seed(42) # set the random seed for reproducibility
random_binomial <- rbinom(10, size = 10, prob = 0.5)

print(random_binomial)

# Constructing a Confidence Interval --------------------------------------

library(tidyverse)

wine <- read_csv("data/wine.csv")

sample_mean <- mean(wine$volatile_acidity) # sample mean

sample_n <- length(wine$volatile_acidity) #  sample size

sample_sd <- sd(wine$volatile_acidity) # sample standard deviation

sample_se <- sample_sd/sqrt(sample_n) # sample standard error

confidence_level <- 0.95 # confidence level

critical_value <- qnorm((1-confidence_level)/2, lower.tail = FALSE) # critical value

critical_value <- qt((1-confidence_level)/2, df = sample_n -1, lower.tail = FALSE) # critical value

margin_error <- critical_value*sample_se # margin of error

lower_bound <- sample_mean - margin_error # CI lower bound
upper_bound <- sample_mean + margin_error # CI upper bound

print(c(lower_bound, upper_bound))

# Test statistic for the mean (one-sample) --------------------------------

library(tidyverse)

wine <- read_csv("data/wine.csv")

white_wine <- wine %>% 
    filter(wine_type == "white") # Keep only the white wine rows

# Two-sided t-test result
result <- t.test(x = white_wine$alcohol, 
                 mu = 10.5)

# View result
print(result)

# One sided test

t.test(x = white_wine$alcohol, 
       alternative = "greater",
       mu = 10.5)

# Specify confidence level

t.test(x = white_wine$alcohol,
       mu = 10.5,
       conf.level = 0.90)

# Test statistic for the mean (two sample) --------------------------------

library(tidyverse)

wine <- read_csv("data/wine.csv")

white_wine <- wine %>% 
    filter(wine_type == "white") # Keep only the white wine rows

red_wine <- wine %>% 
    filter(wine_type == "red") # Keep only the white wine rows

result <- t.test(x = white_wine$alcohol, y = red_wine$alcohol,
                 alternative = "greater",
                 var.equal = TRUE)

print(result)

# Paired t-test -----------------------------------------------------------

library(tidyverse)

before_after <- read_csv("data/before_after.csv")

result <- t.test(x = before_after$before, 
                 y = before_after$after,
                 paired = TRUE) # This argument is needed for paired test

print(result)

# Proportion test ---------------------------------------------------------

library(tidyverse)

wine <- read_csv("data/wine.csv")

white_wine <- wine %>% 
    filter(wine_type == "white") # Keep only the white wine rows

successes <- sum(white_wine$quality <= 4) # Number of successes

total_trials <- length(white_wine$quality) # Total number of trials

hypothesized_proportion <- 0.01 # Hypothesized proportion

# Perform the one-sample proportion test
result <- prop.test(x = successes,
                    n = total_trials,
                    p = hypothesized_proportion)

print(result)

# The Kolmogorov-Smirnov (KS) Test  ---------------------------------------

set.seed(42)
random_normal <- rnorm(100, mean = 10, sd = 2)

ks.test(random_normal, "pnorm", 
        mean = 10, sd = 2)

library(tidyverse)

wine <- read_csv("data/wine.csv")

white_wine <- wine %>% 
    filter(wine_type == "white") # Keep only the white wine rows

# Testing for Normality with a specified mean and standard deviation
ks.test(white_wine$alcohol, "pnorm", 
        mean = mean(white_wine$alcohol), sd = sd(white_wine$alcohol))

# Testing for Normality using the Standard Normal distribution
ks.test(white_wine$alcohol, "pnorm")

set.seed(42)
random_normal <- rnorm(100, mean = 10, sd = 2) # Sample from a Normal distribution
random_poisson <- rpois(100, lambda = 5) # Sample from a Poisson distribution

# Do they come from the same underlying distribution?
ks.test(random_normal, random_poisson)

# White wine data
white_wine <- wine %>% 
    filter(wine_type == "white") # Keep only the white wine rows

# Red wine data
red_wine <- wine %>% 
    filter(wine_type == "red") # Keep only the white wine rows

# Does alcohol have the same underlying distribution?
ks.test(white_wine$alcohol, red_wine$alcohol)

# Visualise the data
wine %>% 
    ggplot(mapping = aes(x = alcohol, fill = wine_type)) +
    geom_density(alpha = 0.5)

# Mann-Whitney U test  ----------------------------------------------------

library(tidyverse)

wine <- read_csv("data/wine.csv")

# White wine data
white_wine <- wine %>% 
    filter(wine_type == "white") # Keep only the white wine rows

# Red wine data
red_wine <- wine %>% 
    filter(wine_type == "red") # Keep only the white wine rows

# Does alcohol have the same underlying distribution?
wilcox.test(white_wine$alcohol, red_wine$alcohol)

# F-test ------------------------------------------------------------------

library(tidyverse)

wine <- read_csv("data/wine.csv")

# White wine data
white_wine <- wine %>% 
    filter(wine_type == "white") # Keep only the white wine rows

# Red wine data
red_wine <- wine %>% 
    filter(wine_type == "red") # Keep only the white wine rows

var.test(white_wine$alcohol, red_wine$alcohol)

# One-factor ANOVA --------------------------------------------------------

library(tidyverse)

# Load the modified wine dataset containing 3 groups
wine_anova <- read_csv("data/wine_anova.csv")

# Perform one-factor ANOVA
result <- aov(alcohol ~ wine_type, data = wine_anova)

# Get results from analysis
summary(result)

wine_anova %>% 
    select(alcohol, wine_type) %>% 
    ggplot(mapping = aes(x = wine_type, y = alcohol)) +
    geom_boxplot()

# Fitting a simple linear model -------------------------------------------

library(tidyverse)

wine <- read_csv("data/wine.csv")

wine %>% 
    ggplot(mapping = aes(x = alcohol, y = density)) +
    geom_point() +
    # Fit a linear model without the standard error band
    geom_smooth(method = "lm", se = FALSE)

# Fit a linear regression model: y ~ x 
result <- lm(density ~ alcohol, data = wine)

# Print model
print(result)

# Summary of model
summary(result)
