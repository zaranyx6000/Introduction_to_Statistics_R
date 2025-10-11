# Date: 2025-06^4-10 install libraries
# install.packages("tidyverse")
# install.packages("ggcorrplot")


# Ex. 1 -------------------------------------------------------------------

df <- mtcars

rm(df)

library(readr)

starwars <- read_csv("data/starwars.csv")

date_vec <- seq(from = as.Date("2023-01-01"), to = as.Date("2023-12-31"), by = "day")

starwars <- starwars %>%   mutate(date = sample(date_vec, size = nrow(starwars), replace = TRUE))

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
       mapping =aes(x = weight))+
  geom_histogram(binwidth =5)

ggplot(data =starwars,
       mapping =aes(x =height, y =weight))+
  geom_point()  

# Ex. 7 -------------------------------------------------------------------

starwars %>%
  
  ggplot(mapping =aes(x = homeworld, y = species ))+
  geom_point()

starwars %>%
  filter(weight <200)%>% # Remove weight value point and pass to ggplot
  ggplot(mapping =aes(x =height, y =weight))+
  geom_point()
# Ex. 8 -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
stroke <- read_csv("data/stroke.csv")
summary(stroke$age)

mean(stroke$age, na.rm = TRUE)
max(stroke$age, na.rm = TRUE)
min(stroke$age, na.rm = TRUE)


# rm(list = ls())
# library(tidyverse)
# wine <-read_csv("data/wine.csv")
# mean(wine$alcohol, na.rm =TRUE)
# median(wine$alcohol, na.rm =TRUE)
# max(table(wine$alcohol))))

stroke %>%
  select(fixed_acidity)%>%
  ggplot(mapping =aes(x =fixed_acidity))+# specify x aesthetics
  # add density layer with specification about the fill colour & transparency
  geom_density(colour ="black", fill ="grey", alpha =0.5)

install.packages("random")
library(random)

randomNumbers(n = 1, min = 0, max = 1, col = 1)
randomNumbers(n = 10, min = 0, max = 1, col = 1)


install.packages("openssl")
library(openssl)

# Erzeugt eine echte, systemnahe Zufallszahl (intern über Windows CryptoAPI)
rand_num <- rand_bytes(1)
val <- sum(as.integer(rand_num))  # Beispielhafte Umwandlung
val

set.seed(42)
random_normal <- rnorm(10000, mean = 0, sd = 1) |> 

qgplot(aes(x =x), bins = 30, color = I("black"), fill = I("grey"))


i <- 1  # Initialisierung des Zählers

while (i <= 10) {  # Schleife läuft, solange i kleiner oder gleich 10 ist
  set.seed(42)  # Setze den Seed (liefert immer dieselben Zufallszahlen)
  
  random_normal <- rnorm(10000, mean = 0, sd = 1)
  print(paste("Durchlauf:", i))
  print(random_normal)
  i <- i + 1  # Erhöhe i um 1 (entspricht i++)
}

set.seed(42)# set the random seed for reproducibility
random_binomial <-rbinom(100, size =1, prob =0.5)
print(random_binomial)
ggplot2::qplot(random_binomial,  binwidth =0.5, color =I("black"), fill =I("grey"))

random_binomial <- rbinom(1000, size =6, prob = 0.16)

print(random_binomial)

library(tidyverse)

plot_data <- data.frame(x = random_binomial)

# Bar Plot
plot_data %>% 
  mutate(x = factor(x)) %>% 
  ggplot(mapping = aes(x = x)) +
  geom_bar()

random_poisson <- rpois(1000, 10)

print(random_poisson)

library(tidyverse)

plot_data <- data.frame(x = random_poisson)

# Bar Plot
plot_data %>% 
  mutate(x = factor(x)) %>% 
  ggplot(mapping = aes(x = x)) +
  geom_bar()
