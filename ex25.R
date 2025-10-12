library(tidyverse)

stroke <- read_csv("data/stroke.csv") |> 
  drop_na()

  stroke$gender <- as.factor(stroke$gender)
  stroke |> filter(gender %in% c("Male", "Female")) 

 

t_res <- t.test(avg_glucose_level ~ gender,
                data = stroke,, alternative = "two.sided",
                var.equal = FALSE) |> 
print()

