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
  

