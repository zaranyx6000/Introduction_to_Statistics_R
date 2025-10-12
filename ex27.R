library(tidyverse)
stroke <-read_csv("data/stroke.csv") |> 
  drop_na()

str(stroke)


stroke_male <-stroke %>%
  filter(gender =="male")# Keep only the white wine rows
successes <-sum(stroke_male$had_stroke >= 1)# Number of successes
total_trials <-nrow(stroke_male)# Total number of trials
hypothesized_proportion <-0.05# Hypothesized proportion
# Perform the one-sample proportion test
result <-prop.test(x =successes,
                   n =total_trials,
                   p =hypothesized_proportion,
                   alternative ="greater")
print(result)
