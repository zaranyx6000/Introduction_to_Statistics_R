library(tidyverse)
library(zaranyx)

stroke <- read.csv("data/stroke.csv") |> 
  drop_na()

stroke_hypertension_1 <- stroke |> 
  filter(hypertension == 1) |> 
  select(-hypertension)

stroke_hypertension_0 <- stroke |> 
  filter(hypertension == 0) |> 
  select(-hypertension)





sample_mean <-mean(stroke$avg_glucose_level)# sample mean
sample_n <-length(stroke$avg_glucose_level)# sample size
sample_sd <-sd(stroke$avg_glucose_level)# sample standard deviation
sample_se <-sample_sd/sqrt(sample_n)# sample standard error
confidence_level <-0.95# confidence level
critical_value <-qnorm((1-confidence_level)/2, lower.tail =FALSE)# critical value
margin_error <-critical_value*sample_se # margin of error
lower_bound <-sample_mean -margin_error # CI lower bound
upper_bound <-sample_mean +margin_error # CI upper bound

ci_mean(stroke$avg_glucose_level, 0.95,FALSE)



sample_mean <-mean(stroke_hypertension_1$avg_glucose_level)# sample mean
sample_n <-length(stroke_hypertension_1$avg_glucose_level)# sample size
sample_sd <-sd(stroke_hypertension_1$avg_glucose_level)# sample standard deviation
sample_se <-sample_sd/sqrt(sample_n)# sample standard error
confidence_level <-0.95# confidence level
critical_value <-qnorm((1-confidence_level)/2, lower.tail =FALSE)# critical value
margin_error <-critical_value*sample_se # margin of error
lower_bound_H1 <-sample_mean -margin_error # CI lower bound
upper_bound_H1 <-sample_mean +margin_error # CI upper bound

sample_mean <-mean(stroke_hypertension_0$avg_glucose_level)# sample mean
sample_n <-length(stroke_hypertension_0$avg_glucose_level)# sample size
sample_sd <-sd(stroke_hypertension_0$avg_glucose_level)# sample standard deviation
sample_se <-sample_sd/sqrt(sample_n)# sample standard error
confidence_level <-0.95# confidence level
critical_value <-qnorm((1-confidence_level)/2, lower.tail =FALSE)# critical value
margin_error <-critical_value*sample_se # margin of error
lower_bound_H0 <-sample_mean -margin_error # CI lower bound
upper_bound_H0 <-sample_mean +margin_error # CI upper bound

ci_data <- tibble(
  Modell = c("Whole Data-Set", "Hypertension High", "Hypertension Low"),
  Lower  = c(lower_bound, lower_bound_H1, lower_bound_H0),
  Upper  = c(upper_bound, upper_bound_H1, upper_bound_H0)
) %>%
  mutate(Mittelwert = (Lower + Upper) / 2)

# Kontrolle: Daten anzeigen
print(ci_data)
# Plot erstellen
ggplot(ci_data, aes(x = Modell, y = Mittelwert)) +
  geom_point(size = 4, color = "steelblue") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                width = 0.2, color = "darkblue", size = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Comparison of Three Confidence Intervals",
    y = "Value (e.g. Mean or Effect Size)",
    x = ""
  ) +
  coord_flip()

