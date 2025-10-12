
# Ex. 1 -------------------------------------------------------------------

df <- mtcars

rm(df)

library(readr)

starwars <- read_csv("data/starwars.csv")

date_vec <- seq(from = as.Date("2023-01-01"), to = as.Date("2023-12-31"), by = "day")

# Ex. 2 -------------------------------------------------------------------

library(tidyverse)

starwars <- read_csv("data/starwars.csv")

df <- select(starwars, name, height, eye_color, hair_color)

df <- select(starwars, -name, -height)

df <- select(starwars, ends_with("color"))

# Ex. 3 -------------------------------------------------------------------

library(tidyverse)

starwars <- read_csv("data/starwars.csv")

df <- filter(starwars, weight > 50)

df <- filter(starwars, weight > 50 & height >= 175)

# Ex. 4 -------------------------------------------------------------------

library(tidyverse)

starwars <- read_csv("data/starwars.csv")

df <- mutate(starwars, weight_g = weight*1000)

# Ex. 5 -------------------------------------------------------------------

library(tidyverse)

starwars <- read_csv("data/starwars.csv")

df <- starwars %>% 
    select(name, height, eye_color, weight) %>% 
    filter(eye_color == "blue") %>% 
    mutate(weight_g = weight*1000)

# Ex. 6 -------------------------------------------------------------------

library(tidyverse)

starwars <- read_csv("data/starwars.csv")

ggplot(data = starwars,
       mapping = aes(x = weight)) +
    geom_histogram(binwidth = 5) # specify binwidth

# Ex. 7 ------------------------------------------------------------------

library(tidyverse)

starwars <- read_csv("data/starwars.csv")

starwars %>% 
    filter(weight < 200) %>% # Remove weight value point and pass to ggplot
    ggplot(mapping = aes(x = height, y = weight)) +
    geom_point()

# Group Exercise ----------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv")

View(stroke)

nrow(stroke)
ncol(stroke)
names(stroke)
summary(stroke)

df <- stroke %>% 
    drop_na()

df <- stroke %>% 
    drop_na() %>% 
    filter(gender == "male" & age > 50 & smoking_status == "smokes")

# Ex. 8 -------------------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv")

summary(stroke$age)

min(stroke$age, na.rm = TRUE)

max(stroke$age, na.rm = TRUE)

# Or from now on you can use drop_na() and not have to worry about NAs

# Ex. 9 -------------------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv")

mean(stroke$age, na.rm = TRUE)

median(stroke$age, na.rm = TRUE)

stroke %>% 
    group_by(age) %>% # Group by the variable in question
    summarise(counts = n()) %>% # Count the number of observations
    filter(counts == max(counts)) %>% # Keep the maximum count
    pull(age) # Pull the variable name

# Ex. 10 ------------------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv")

range(stroke$age, na.rm = TRUE) # range of variable

IQR(stroke$age, na.rm = TRUE) # interquartile range of variable

var(stroke$age, na.rm = TRUE) # variance of variable

sd(stroke$age, na.rm = TRUE) # standard deviation of variable

# Ex. 11 ------------------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv")

quantile(stroke$age, probs = seq(0, 1, 0.25), na.rm = TRUE) # quartiles

quantile(stroke$age, probs = seq(0, 1, 0.01), na.rm = TRUE) # percentiles

quantile(stroke$age, probs = seq(0, 1, 0.10), na.rm = TRUE) # deciles

# Ex. 12 ------------------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv")

mean_value <- mean(stroke$avg_glucose_level, na.rm = TRUE)

Q1_value <- quantile(stroke$avg_glucose_level, probs = 0.25, na.rm = TRUE) # Lower Quartile

median_value <- quantile(stroke$avg_glucose_level, probs = 0.50, na.rm = TRUE) # Median

Q3_value <- quantile(stroke$avg_glucose_level, probs = 0.75, na.rm = TRUE) # Upper Quartile

stroke %>%
    select(avg_glucose_level) %>%
    ggplot(mapping = aes(x = avg_glucose_level)) + # specify x aesthetics
    # add histogram layer
    geom_histogram(binwidth = 5, colour = "black", fill = "grey") +
    # now add vertical line for mean
    geom_vline(xintercept = mean_value, lwd = 1, colour = "red") +
    # now add vertical line for median
    geom_vline(xintercept = median_value, lwd = 1, colour = "blue", linetype = "solid") +
    geom_vline(xintercept = Q1_value, lwd = 1, colour = "green", linetype = "dashed") +
    geom_vline(xintercept = Q3_value, lwd = 1, colour = "green", linetype = "dashed")

# Ex. 13 ------------------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv")

stroke %>%
    select(avg_glucose_level) %>% 
    ggplot(mapping = aes(x = avg_glucose_level)) + # specify x aesthetics
    # add boxplot layer with specification about the outliers
    geom_boxplot(outlier.colour = "red", outlier.shape = 4)

# Ex. 14 ------------------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv")

stroke %>%
    select(avg_glucose_level) %>% 
    ggplot(mapping = aes(x = avg_glucose_level)) + # specify x aesthetics
    # add density layer with specification about the filling colour and transparency
    geom_density(colour = "black", fill = "grey", alpha = 0.5)

# Ex. 15 ------------------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv")

stroke %>% 
    select(smoking_status) %>% 
    # convert quality as a factor
    mutate(smoking_status = factor(smoking_status)) %>% 
    # specify x and fill aesthetics
    ggplot(mapping = aes(x = smoking_status)) + 
    # add bar plot layer with specification on how the bars are placed
    geom_bar(position = "dodge")

# Ex. 16 ------------------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv")

stroke %>% 
    select(age, avg_glucose_level) %>% 
    # specify x and y aesthetics
    ggplot(mapping = aes(x = age, y = avg_glucose_level)) +
    # add points geometry layer
    geom_point() +
    # add a smoothed line or curve
    geom_smooth()

# Ex. 17 ------------------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv")

# Boxplot
stroke %>% 
    select(avg_glucose_level, had_stroke) %>% 
    mutate(had_stroke = factor(had_stroke)) %>%  # convert to factor
    # specify x and y aesthetics
    ggplot(mapping = aes(x = avg_glucose_level, fill = had_stroke)) +
    # add boxplot layer
    geom_boxplot()

# Histogram
stroke %>% 
    select(avg_glucose_level, had_stroke) %>% 
    mutate(had_stroke = factor(had_stroke)) %>%  # convert to factor
    # specify x and y aesthetics
    ggplot(mapping = aes(x = avg_glucose_level, fill = had_stroke)) +
    # add boxplot layer
    geom_histogram()

# Density plot
stroke %>% 
    select(avg_glucose_level, had_stroke) %>% 
    mutate(had_stroke = factor(had_stroke)) %>%  # convert to factor
    # specify x and y aesthetics
    ggplot(mapping = aes(x = avg_glucose_level, fill = had_stroke)) +
    # add boxplot layer
    geom_density(alpha = 0.5)

# Group Challenges --------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv")

# Group Challenge 1
plot_data <- stroke %>% 
    select(had_stroke, work_type) %>% 
    drop_na() %>% 
    filter(had_stroke == 1) %>% 
    mutate(had_stroke = factor(had_stroke)) %>% 
    group_by(had_stroke, work_type) %>% 
    summarise(count = n()) %>% 
    ungroup()

plot_data %>%
    ggplot(mapping = aes(x = work_type, y = count, fill = had_stroke)) +
    geom_col(position = "dodge")

# Group Challenge 2

plot_data <- stroke %>% 
    select(age, avg_glucose_level, bmi) %>%
    drop_na() %>% 
    cor()

library(ggcorrplot)

ggcorrplot(plot_data,
           type = "lower",
           lab = TRUE, 
           lab_size = 5)

# Ex. 18 ------------------------------------------------------------------

random_normal <- rnorm(10000, mean = 0, sd = 1)

set.seed(42)
random_normal <- rnorm(1000, mean = 0, sd = 1)

library(tidyverse)

plot_data <- data.frame(x = random_normal)

# Histogram
plot_data %>% 
    ggplot(mapping = aes(x = x)) +
    geom_histogram(binwidth = 0.1) # specify binwidth

# Density
plot_data %>% 
    ggplot(mapping = aes(x = x)) +
    geom_density()

# Ex. 19 ------------------------------------------------------------------

set.seed(42) # set the random seed for reproducibility
random_poisson <- rpois(1000, lambda = 2)

print(random_poisson)

library(tidyverse)

plot_data <- data.frame(x = random_poisson)

# Bar Plot
plot_data %>% 
    mutate(x = factor(x)) %>% 
    ggplot(mapping = aes(x = x)) +
    geom_bar()

# Ex. 20 ------------------------------------------------------------------

set.seed(42) # set the random seed for reproducibility
random_binomial <- rbinom(1000, size = 10, prob = 0.1)

print(random_binomial)

library(tidyverse)

plot_data <- data.frame(x = random_binomial)

# Bar Plot
plot_data %>% 
    mutate(x = factor(x)) %>% 
    ggplot(mapping = aes(x = x)) +
    geom_bar()

# Ex. 21 ------------------------------------------------------------------

library(tidyverse)

# Poisson distribution approximated to Normal
set.seed(42) # set the random seed for reproducibility
random_poisson <- rpois(1000, lambda = 5)
random_poisson <- rpois(1000, lambda = 20)
random_poisson <- rpois(1000, lambda = 100)

plot_data <- data.frame(x = random_poisson)

plot_data %>% 
    mutate(x = factor(x)) %>% 
    ggplot(mapping = aes(x = x)) +
    geom_bar()

# Binomial distribution approximated to Normal
set.seed(42) # set the random seed for reproducibility
random_binomial <- rbinom(1000, size = 10, prob = 0.1)
random_binomial <- rbinom(1000, size = 100, prob = 0.1)
random_binomial <- rbinom(1000, size = 1000, prob = 0.1)

plot_data <- data.frame(x = random_binomial)

plot_data %>% 
    mutate(x = factor(x)) %>% 
    ggplot(mapping = aes(x = x)) +
    geom_bar()

# Ex. 22 ------------------------------------------------------------------

library(tidyverse)

set.seed(42) # set the random seed for reproducibility

# For 1 sample
# Sample size of 50 having a Binomial distribution with n = 20 and p = 0.5
sample <- rbinom(n = 50, size = 20, prob = 0.5)
sample_mean <- mean(sample)

# For 1000 samples
set.seed(42)
# Creates a matrix with each sample in a column, i.e. 1000 columns of 50 rows each
sample_all_data <- replicate(n = 1000, rbinom(n = 50, size = 20, prob = 0.5))

set.seed(42)
sample_means <- replicate(n = 1000, mean(rbinom(50, size = 20, prob = 0.5)))

# CLT
20*0.5 # The theoretical mean - this is mu
(20*0.5*(1-0.5))/50 # The theoretical variance

mean(sample_means)
var(sample_means)

plot_data <- data.frame(x = sample_means)

plot_data %>% 
    ggplot(mapping = aes(x = x)) +
    geom_histogram(binwidth = 0.05) # specify binwidth

plot_data %>% 
    ggplot(mapping = aes(x = x)) +
    geom_density()

# Group 1 exercise --------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv") %>% 
    filter(!is.na(avg_glucose_level))

sample_mean <- mean(stroke$avg_glucose_level) # sample mean
sample_n <- length(stroke$avg_glucose_level) #  sample size
sample_sd <- sd(stroke$avg_glucose_level) # sample standard deviation
sample_se <- sample_sd/sqrt(sample_n) # sample standard error
confidence_level <- 0.99 # confidence level
critical_value <- qnorm((1-confidence_level)/2, lower.tail = FALSE) # critical value
#critical_value <- qt((1-confidence_level)/2, df = sample_n -1, lower.tail = FALSE) # critical value
margin_error <- critical_value*sample_se # margin of error

lower_bound <- sample_mean - margin_error # CI lower bound
upper_bound <- sample_mean + margin_error # CI upper bound

print(c(lower_bound, upper_bound))

# We expect 99% of the confidence intervals (from many samples)
# to contain the true population mean for average glucose level in blood 
# and the value to lie between 106.4481 and 110.9118 mg/dL.“

# Group 2 exercise --------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv") %>% 
    #filter(hypertension == 1)
    filter(hypertension == 0)

sample_prop <- mean(stroke$had_stroke) # sample mean
sample_n <- length(stroke$had_stroke) #  sample size
sample_se <- sqrt(sample_prop*(1-sample_prop)/sample_n) # sample standard error
confidence_level <- 0.95 # confidence level
critical_value <- qnorm((1-confidence_level)/2, lower.tail = FALSE) # critical value
margin_error <- critical_value*sample_se # margin of error

lower_bound <- sample_prop - margin_error # CI lower bound
upper_bound <- sample_prop + margin_error # CI upper bound

print(c(lower_bound, upper_bound))

# Ex. 23 ------------------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv") %>% 
    drop_na()

result <- t.test(x = stroke$avg_glucose_level, 
                 mu = 100)

print(result)

# Ex. 24 ------------------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv") %>% 
    drop_na()

result <- t.test(x = stroke$avg_glucose_level,
                 alternative = "greater",
                 mu = 100)

result <- t.test(x = stroke$avg_glucose_level,
                 alternative = "less",
                 mu = 100)

print(result)

# Ex. 25 ------------------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv") %>% 
    drop_na()

male_data <- stroke %>% 
    filter(gender == "male")

female_data <- stroke %>% 
    filter(gender == "female")

result <- t.test(x = male_data$avg_glucose_level,
                 y = female_data$avg_glucose_level,
                 alternative = "two.sided",
                 var.equal = TRUE)

print(result)

# Other test for stroke patients
had_stroke <- stroke %>% 
    filter(had_stroke == 1)

not_had_stroke <- stroke %>% 
    filter(had_stroke == 0)

result <- t.test(x = had_stroke$avg_glucose_level,
                 y = not_had_stroke$avg_glucose_level,
                 alternative = "greater")

print(result)

# Ex. 26 ------------------------------------------------------------------

library(tidyverse)

before_after <- read_csv("data/before_after.csv")

result <- t.test(x = before_after$before, 
                 y = before_after$after,
                 alternative = "less",
                 paired = TRUE)

print(result)

# Ex. 27 ------------------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv") %>% 
    drop_na()

male_data <- stroke %>% 
    filter(gender == "male")

successes <- sum(male_data$had_stroke) # Number of successes

total_trials <- length(male_data$had_stroke)

hypothesized_proportion <- 0.05 # Hypothesized proportion

result <- prop.test(x = successes,
                    n = total_trials,
                    p = hypothesized_proportion)

print(result)

# Ex. 28 ------------------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv") %>% 
    drop_na()

ks.test(data, "pnorm", 
        mean = mean(stroke$avg_glucose_level), sd = sd(stroke$avg_glucose_level))

before_after <- read_csv("data/before_after.csv") %>% 
    mutate(delta = before - after)

ks.test(data, "pnorm", 
        mean = mean(before_after$delta), sd = sd(before_after$delta))

# Ex. 29 ------------------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv") %>% 
    drop_na()

male_data <- stroke %>% 
    filter(gender == "male")

female_data <- stroke %>% 
    filter(gender == "female")

ks.test(male_data$avg_glucose_level, female_data$avg_glucose_level)

# Visualise the data
stroke %>% 
    ggplot(mapping = aes(x = avg_glucose_level, fill = gender)) +
    geom_density(alpha = 0.5)

had_stroke <- stroke %>% 
    filter(had_stroke == 1)

not_had_stroke <- stroke %>% 
    filter(had_stroke == 0)

ks.test(had_stroke$avg_glucose_level, not_had_stroke$avg_glucose_level)

stroke %>% 
    mutate(had_stroke = factor(had_stroke)) %>% 
    ggplot(mapping = aes(x = avg_glucose_level, fill = had_stroke)) +
    geom_density(alpha = 0.5)

# Ex. 30 ------------------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv") %>% 
    drop_na()

male_data <- stroke %>% 
    filter(gender == "male")

female_data <- stroke %>% 
    filter(gender == "female")

had_stroke <- stroke %>% 
    filter(had_stroke == 1)

not_had_stroke <- stroke %>% 
    filter(had_stroke == 0)

wilcox.test(male_data$avg_glucose_level, female_data$avg_glucose_level)

wilcox.test(had_stroke$avg_glucose_level, not_had_stroke$avg_glucose_level)

# Ex. 31 ------------------------------------------------------------------

library(tidyverse)

stroke <- read_csv("data/stroke.csv") %>% 
    drop_na()

had_stroke <- stroke %>% 
    filter(had_stroke == 1)

not_had_stroke <- stroke %>% 
    filter(had_stroke == 0)

var.test(had_stroke$avg_glucose_level, not_had_stroke$avg_glucose_level)

# Ex. 32 ------------------------------------------------------------------

library(tidyverse)

diet <- read_csv("data/diet.csv") %>% 
    drop_na() %>% 
    mutate(diet = factor(diet)) %>% 
    mutate(weight_loss = post_diet_weight - pre_diet_weight)

# Perform one-factor ANOVA
result <- aov(weight_loss ~ diet, data = diet)

# Get results from analysis
summary(result)

diet %>% 
    select(weight_loss, diet) %>% 
    ggplot(mapping = aes(x = diet, y = weight_loss)) +
    geom_boxplot()

# Ex. 33 ------------------------------------------------------------------

library(tidyverse)

diet <- read_csv("data/diet.csv") %>% 
    drop_na() %>% 
    mutate(diet = factor(diet)) %>% 
    mutate(gender = factor(gender)) %>% 
    mutate(weight_loss = post_diet_weight - pre_diet_weight)

# Simple Linear Regression Model
result <- lm(weight_loss ~ diet, data = diet)

summary(result)

# Multiple Linear Regression Model
result <- lm(weight_loss ~ gender + age + diet, data = diet)

summary(result)

