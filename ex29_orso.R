library(tidyverse)

before_after <- read_csv("data/before_after.csv")

t.test(x = before_after$before, y = before_after$after, alternative = "less", paired = TRUE)
