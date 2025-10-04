# Date: 2025-06^4-10 install libraries
# install.packages("tidyverse")
# install.packages("ggcorrplot")


# Description: Load libraries ----
library(tidyverse)
library(ggcorrplot)

# Exercise 1 ----
df <- mtcars
rm(df)
df <- read_csv("starwars.csv") |> mutate(index = seq(2023,2023,1))

