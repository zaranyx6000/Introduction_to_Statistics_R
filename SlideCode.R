# ==============================================
# Introduction to Statistics using R — Code Index
# Source: Course slides (compiled by slide)
# Note: Each section starts with: Slide <number> — <title>
# ==============================================

# --- Slide 12 — Systems check ---
install.packages(c("tidyverse", "ggcorrplot"))
# Include all dependencies. Select "NO" when asked to install from source
library(tidyverse)     # Load tidyverse package
library(ggcorrplot)    # Load ggcorrplot package
sessionInfo()          # Check version information about R and OS

# --- Slide 18 — Creating R objects ---
x <- 42
print(x)

# --- Slide 20 — Creating R objects of specific data types ---
x <- c(1, 2, 3, 4)
y <- c(TRUE, FALSE)

# --- Slide 22 — Remove R objects ---
rm(list = ls())   # Removes all the objects from the workspace
rm(x)             # Removes the object x from the workspace

# --- Slide 25 — Load a CSV file into R using command line ---
library(readr)
starwars <- read_csv("starwars.csv")  # Replace with actual path

view(starwars)

# --- Slide 31 — Select columns from your data ---
library(tidyverse)
df <- select(starwars, name, height)  # Select columns name and height

# --- Slide 35 — Filter rows from your data ---
library(tidyverse)
df <- filter(starwars, height >= 175)  # Keep rows with height >= 175

# --- Slide 38 — Mutate your data ---
library(tidyverse)
# Create a new column for height in metres
df <- mutate(starwars, height_m = height/100)

# --- Slide 43 — Pipe workflow demo ---
df <- starwars %>%
  select(name, height) %>%
  filter(height >= 175) %>%
  mutate(height_m = height/100)

# --- Slide 47 — Build a simple plot (initial object) ---
ggplot(data = starwars, mapping = aes(x = height))

# --- Slide 51 — Build a simple plot (histogram) ---
ggplot(data = starwars, mapping = aes(x = height)) +
  geom_histogram(binwidth = 5)  # specify binwidth

# --- Slide 54 — Build a simple plot (scatter plot) ---
ggplot(data = starwars, mapping = aes(x = height, y = weight)) +
  geom_point()

# --- Slide 56 — Build a simple plot (filter then plot) ---
starwars %>%
  filter(weight < 200) %>%  # Remove extreme weight value and pass to ggplot
  ggplot(mapping = aes(x = height, y = weight)) +
  geom_point()

# --- Slide 75 — Five-number summary (wine) ---
library(tidyverse)
wine <- read_csv("data/wine.csv")
summary(wine$alcohol)
min(wine$alcohol)
max(wine$alcohol)

# --- Slide 77 — Measures of central tendency ---
library(tidyverse)
wine <- read_csv("data/wine.csv")
mean(wine$alcohol, na.rm = TRUE)
median(wine$alcohol, na.rm = TRUE)

# --- Slide 78 — A note on the mode (using dplyr) ---
wine %>%
  group_by(quality) %>%            # Group by the variable in question
  summarise(counts = n()) %>%      # Count the number of observations
  filter(counts == max(counts)) %>%# Keep the maximum count
  pull(quality)                    # Pull the variable name

# --- Slide 80 — Measures of spread / variability ---
range(wine$alcohol, na.rm = TRUE)  # range of variable
IQR(wine$alcohol, na.rm = TRUE)    # interquartile range of variable
var(wine$alcohol, na.rm = TRUE)    # variance of variable
sd(wine$alcohol, na.rm = TRUE)     # standard deviation of variable

# --- Slide 83 — Quantiles ---
quantile(wine$alcohol, na.rm = TRUE)  # Default: quartiles
# Specify a set of probabilities to calculate the quantiles
quantile(wine$alcohol, probs = seq(from = 0, to = 1, by = 0.25), na.rm = TRUE)

# --- Slide 90 — Demo histogram with mean/median lines ---
mean_value <- mean(wine$fixed_acidity)
median_value <- median(wine$fixed_acidity)
wine %>%
  select(fixed_acidity) %>%
  ggplot(mapping = aes(x = fixed_acidity)) +                  # specify x aesthetics
  geom_histogram(binwidth = 0.5, colour = "black", fill = "grey") +  # histogram
  geom_vline(xintercept = mean_value, lwd = 1, colour = "red") +     # mean line
  geom_vline(xintercept = median_value, lwd = 1, colour = "blue")    # median line

# --- Slide 94 — Demo boxplot ---
wine %>%
  select(fixed_acidity) %>%
  ggplot(mapping = aes(x = fixed_acidity)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 4)

# --- Slide 98 — Demo density plot ---
wine %>%
  select(fixed_acidity) %>%
  ggplot(mapping = aes(x = fixed_acidity)) +
  geom_density(colour = "black", fill = "grey", alpha = 0.5)

# --- Slide 103 — Bar plot (basic) ---
wine %>%
  select(quality) %>%
  ggplot(mapping = aes(x = quality)) +
  geom_bar()

# --- Slide 105 — Bar plot with factor conversion ---
wine %>%
  select(quality) %>%
  mutate(quality = factor(quality)) %>%
  ggplot(mapping = aes(x = quality)) +
  geom_bar(position = "dodge")

# --- Slide 111 — Scatter plot (alcohol vs density) ---
wine %>%
  select(alcohol, density) %>%
  ggplot(mapping = aes(x = alcohol, y = density)) +
  geom_point()

# --- Slide 115 — Box plot (two variants) ---
# Variant A: y aesthetic
wine %>%
  select(fixed_acidity, wine_type) %>%
  mutate(wine_type = factor(wine_type)) %>%
  ggplot(mapping = aes(x = fixed_acidity, y = wine_type)) +
  geom_boxplot()

# Variant B: fill aesthetic
wine %>%
  select(fixed_acidity, wine_type) %>%
  mutate(wine_type = factor(wine_type)) %>%
  ggplot(mapping = aes(x = fixed_acidity, fill = wine_type)) +
  geom_boxplot()

# --- Slide 119 — Bar plot of two categorical variables ---
plot_data <- wine %>%
  select(quality, wine_type) %>%
  mutate(quality = factor(quality)) %>%
  mutate(wine_type = factor(wine_type)) %>%
  group_by(quality, wine_type) %>%
  summarise(count = n()) %>%
  ungroup()

plot_data %>%
  ggplot(mapping = aes(x = quality, y = count, fill = wine_type)) +
  geom_col(position = "dodge")

# --- Slide 122 — Correlation plot (ggcorrplot) ---
library(ggcorrplot)
plot_data <- df %>%                       # replace df with your data frame
  select(where(is.numeric)) %>%           # select only numerical columns
  cor()
ggcorrplot(plot_data, type = "lower", lab = TRUE, lab_size = 5)

# --- Slide 146 — Random numbers from a Normal distribution (reproducible) ---
set.seed(42)  # set the random seed for reproducibility
random_normal <- rnorm(10, mean = 10, sd = 2)
print(random_normal)

# --- Slide 151 — Random numbers from a Poisson distribution (reproducible) ---
set.seed(42)  # set the random seed for reproducibility
random_poisson <- rpois(10, lambda = 5)
print(random_poisson)

# --- Slide 156 — Random numbers from a Binomial distribution (reproducible) ---
set.seed(42)  # set the random seed for reproducibility
random_binomial <- rbinom(10, size = 10, prob = 0.5)
print(random_binomial)

# --- Slide 173 — Confidence interval (demo) ---
library(tidyverse)
wine <- read_csv("data/wine.csv")
sample_mean <- mean(wine$volatile_acidity)                 # sample mean
sample_n    <- length(wine$volatile_acidity)               # sample size
sample_sd   <- sd(wine$volatile_acidity)                   # sample standard deviation
sample_se   <- sample_sd/sqrt(sample_n)                    # sample standard error
confidence_level <- 0.95                                   # confidence level
critical_value   <- qnorm((1 - confidence_level)/2, lower.tail = FALSE) # z* critical value
margin_error     <- critical_value * sample_se             # margin of error
lower_bound <- sample_mean - margin_error                  # CI lower bound
upper_bound <- sample_mean + margin_error                  # CI upper bound
print(c(lower_bound, upper_bound))

# --- Slide 170/171 — Using t critical value (alternative) ---
# If population variance is unknown, prefer Student's t distribution:
dfree <- sample_n - 1
t_crit <- qt((1 - confidence_level)/2, df = dfree, lower.tail = FALSE)
t_lower <- sample_mean - t_crit * sample_se
t_upper <- sample_mean + t_crit * sample_se
print(c(t_lower, t_upper))

# ==============================================
# End of compiled code snippets
# ==============================================
""")

path = "/mnt/data/Intro_Stats_R_code_by_slide.R"
with open(path, "w", encoding="utf-8") as f:
    f.write(code)