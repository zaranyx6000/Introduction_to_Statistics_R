
library(tidyverse)

before_after <- read_csv("data/before_after.csv") |> 
 drop_na() |> 
  mutate(ba_delta = after - before)

stroke <- read_csv("data/stroke.csv") |> 
  drop_na()

shapiro.test(before_after$ba_delta)
shapiro.test(stroke$avg_glucose_level)
