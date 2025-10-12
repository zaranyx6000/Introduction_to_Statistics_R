
library(tidyverse)

stroke <- read_csv("data/stroke.csv") %>% 
  drop_na()

male_data <- stroke %>% 
  filter(gender == "male")

female_data <- stroke %>% 
  filter(gender == "female")

had_stroke <- stroke %>% 
  filter(had_stroke == 1)

not_had_stroke <- stroke %>% 
  filter(had_stroke == 0)

wilcox.test(female_data$had_stroke, male_data$had_stroke)
wilcox.test(had_stroke$age, not_had_stroke$age)
