library(tidyverse)
library(ggthemes)
#Load the dataset
melanoma_data <- read.csv("C:/Users/dekea/Downloads/melanoma.csv")
#View the data
head(melanoma_data)
glimpse(melanoma_data)
#remove the 1st column
melanoma_data <- melanoma_data |>  select(-1)

#Recode 'sex', 'ulcer' and 'status'
melanoma_data <- melanoma_data |>  
  mutate(
    sex = recode(sex , '1' = 'Male', '0' = 'Female'),
    ulcer = recode(ulcer, '1' = 'Present', '0' = 'Absent'),
    status = recode(status, '1' = 'Died', '2' = 'Alive', '3' = 'Died Unrelated'))

#View the updated data
View(melanoma_data)
head(melanoma_data)

#Descriptive statistics of the data
melanoma_summary <- melanoma_data |> select_if(is.numeric) |> summary()
melanoma_summary
var(melanoma_data $time)
var(melanoma_data $ age)
var(melanoma_data $ thickness)
sd(melanoma_data $ time)
sd(melanoma_data $ age)
sd(melanoma_data $ thickness)
#create frequency table for 'sex'. 'ulcer' and 'status'
table_status <- table(melanoma_data $status)
table_sex <- table(melanoma_data $ sex)
table_ulcer <- table(melanoma_data $ ulcer)
table_status
table_sex
table_ulcer
par(mfrow=c(2,2))
hist( melanoma_data $thickness)
hist(melanoma_data $age)
hist(melanoma_data $ year)
hist(melanoma_data $time)
#view boxplot
boxplot(melanoma_data$ thickness ~ melanoma_data $ year)


# Create a grouped boxplot
ggplot(melanoma_data, aes(x = sex, y = thickness, fill = as.factor(ulcer))) +
  geom_boxplot() +
  labs(title = "Relationship between Thickness and Ulcer Grouped by Sex",
       x = "Sex",
       y = "Thickness",
       fill = "Ulcer") +
  theme_minimal()

attach(melanoma_data)
cor(time,  thickness, method = 'pearson')
plot(time, thickness)
model_1=lm(formula = time ~ thickness)
model_1
summary(model_1)

cor(time,  age, method = 'pearson')
plot(time, age)
model_2=lm(formula = time ~ age)
model_2
summary(model_2)

cor(  thickness, age, method = 'pearson')
plot( thickness, age)
model_3=lm(formula = thickness ~ age)
model_3
summary(model_3)
qplot(x= ulcer, y = thickness,fill = sex, 
      geom = "boxplot", data = melanoma_data,
      xlab = "Ulcer",
      ylab = "Thickness(mm)")
present <- filter(melanoma_data, ulcer =='Present')
present
mean(present $ thickness)

melanoma_data |> 
 group_by(ulcer) |> 
  summarize(mean_thickness = mean(thickness),
              sd_thickness = sd(thickness))

melanoma_data |> 
  group_by(sex) |> 
  summarize(mean_age = mean(age),
            sd_age = sd(age),
    mean_thickness = mean(thickness),
            sd_thickness = sd(thickness))
thickness_t_test <- t.test(thickness ~ sex, data = melanoma_data)
thickness_t_test
age_t_test <- t.test(age ~ sex, data = melanoma_data)
age_t_test
time_t_test <- t.test(time ~ sex, data = melanoma_data)
time_t_test
p_thickness <- ggplot(data = melanoma_data, aes(sample = thickness))
p_thickness + stat_qq() + stat_qq_line() + facet_grid(. ~ sex)
p_age <- ggplot(data = melanoma_data, aes(sample = age))
p_age + stat_qq() + stat_qq_line() + facet_grid(. ~ sex)
p_time <- ggplot(data = melanoma_data, aes(sample = time))
p_time + stat_qq() + stat_qq_line() + facet_grid(. ~ sex)

                         
