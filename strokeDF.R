stroke <- read_csv("data/stroke.csv")
nrow(stroke)
ncol(stroke)
names(stroke)
summary(stroke)
str(stroke)
view(stroke)



stroke_mS50 <- stroke |> 
    drop_na() |> 
  filter(gender == "male", age > 50, smoking_status == "smokes")
view(stroke_mS50)
nrow(stroke_mS50)


rm(list = ls())
library(tidyverse)
stroke <-read_csv("data/stroke.csv")
mean(stroke$age, na.rm =TRUE)
median(stroke$age, na.rm =TRUE)


range(stroke$age, na.rm =TRUE)
IQR(stroke$age, na.rm =TRUE)
var(stroke$age, na.rm =TRUE)
sd(stroke$age, na.rm =TRUE)
quantile(stroke$age, na.rm =TRUE)
quantile(stroke$age, probs =c(0.25, 0.5, 0.75), na.rm =TRUE)
quantile(stroke$age, probs = seq(0,1,0.1), na.rm =TRUE)
quantile(stroke$age, probs = seq(0.0,1,0.01), na.rm =TRUE)
# ex. 12 -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
stroke <-read_csv("data/stroke.csv") |> 
  drop_na()

mean_val <- mean(stroke$avg_glucose_level)
median_val <- median(stroke$avg_glucose_level)

stroke |>   select(avg_glucose_level) |> 
  ggplot(mapping = aes(x = avg_glucose_level))+# specify x aesthetics
  # add histogram layer
  geom_histogram(binwidth =0.5, colour ="black", fill ="grey")+
  # now add vertical line for mean
  geom_vline(xintercept =mean_val, lwd =1, colour ="red")+
  # now add vertical line for median
  geom_vline(xintercept =median_val, lwd =1, colour ="blue")+
  geom_vline(xintercept = quantile(stroke$avg_glucose_level, na.rm =TRUE), lwd =1, colour ="green")

# ex. 13 -------------------------------------------------------------------

stroke %>%
  select(avg_glucose_level) %>%
  ggplot(mapping =aes(x =avg_glucose_level))+# specify x aesthetics
  # add boxplot layer with specification about the outliers
  geom_boxplot(outlier.colour ="red", outlier.shape =4)
# ex. 14 -------------------------------------------------------------------
stroke %>%
  select(avg_glucose_level, had_stroke) %>%
  mutate(had_stroke = factor(had_stroke, labels =c("No", "Yes"))) %>%
  ggplot(aes(x =avg_glucose_level,fill = had_stroke))+# specify x aesthetics
  # add density layer with specification about the fill colour & transparency
  geom_density(colour ="black", alpha =0.5)

stroke %>%
  select(avg_glucose_level, had_stroke) %>%
  mutate(had_stroke = factor(had_stroke, labels = c("No", "Yes"))) %>%
  ggplot(aes(x = avg_glucose_level, fill = had_stroke)) +  # fill aesthetic INSIDE aes()
  geom_density(colour = "black", alpha = 0.5)


rm(list = ls())
library(tidyverse)
stroke <-read_csv("data/stroke.csv") |> 
  drop_na()


stroke %>%
  select(smoking_status)%>%
  # convert quality as a factor
  mutate(smoking_status = factor(smoking_status))%>%
  # specify x aesthetics
  ggplot(mapping =aes(x =smoking_status))+
  # add bar plot layer with specification on how the bars are placed
  geom_bar(position ="dodge")

stroke %>%
  select(age, avg_glucose_level)%>%
  # specify x and y aesthetics
  ggplot(mapping =aes(x = age, y = avg_glucose_level))+
  # add points geometry layer
  geom_point() +
  geom_smooth()

# group Challenge1

rm(list = ls())
library(tidyverse)
stroke <-read_csv("data/stroke.csv") |> 
  drop_na()
# view(stroke)
plot_data <- stroke |> 
  select(work_type, had_stroke) |> 
  mutate(work_type = factor(work_type)) |>  # convert to factor
  mutate(had_stroke =factor(had_stroke)) |>  # convert to factor
  group_by(work_type, had_stroke) |> 
  summarise(count =n())%>%
  ungroup() |> 
  filter(had_stroke == 1)

plot_data %>%
  # specify x aesthetics
  ggplot(mapping =aes(x =work_type, y =count, fill =had_stroke))+
  # add bar plot layer
  geom_col(position ="dodge")

 
