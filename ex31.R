library(tidyverse)
stroke <-read_csv("data/stroke.csv") |> 
  drop_na()

str(stroke)
stroked_yes <- stroke |> 
  filter(had_stroke == 1)
stroked_no <- stroke |> 
  filter(had_stroke == 0)

var.test(stroked_yes$avg_glucose_level, stroked_no$avg_glucose_level)
