library(tidyverse)
stroke <-read_csv("data/stroke.csv")

result <-t.test(x =stroke$avg_glucose_level,
                mu =100)
# View result
print(result)
