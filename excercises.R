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






EXERCISE 12

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


