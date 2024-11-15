library(ggplot2)

data <- mtcars

model <- lm(mpg ~ hp, data = data)

summary(model)

# mpg vs. hp regression 
ggplot(data, aes(x = hp, y = mpg)) +
  geom_point(color = "blue") +            
  geom_smooth(method = "lm", color = "red") + 
  labs(title = "Miles per Gallon vs. Horsepower",
       x = "Horsepower",
       y = "Miles per Gallon") +
  theme_minimal()