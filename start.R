# Date: 2025-06^4-10 install libraries
# install.packages("tidyverse")
# install.packages("ggcorrplot")


# Description: Load libraries ----
library(tidyverse)
library(ggcorrplot)

# Exercise 1 ----
df <- mtcars
rm(df)
df <- read_csv("data/starwars.csv") |> mutate(index = seq(2023,2023,1))

# Exercise 2 ----

library(tidyverse)
starwars <- read_csv("data/starwars.csv") |> 
  select(name, height, eye_color, hair_color) |> 
  select(-name, -height) |>
  select(ends_with("color"))

# Exercise 2 from Nico ----  
library(tidyverse)

starwars <- read_csv("data/starwars.csv")

df <- select(starwars, name, height, eye_color, hair_color)

df <- select(starwars, -name, -height)

df <- select(starwars, ends_with("color"))


  

