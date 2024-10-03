data("iris")

plot(iris$Sepal.Length, iris$Petal.Length,
     main = "Sepal Length vs Petal Length",
     xlab = "Sepal Length ",
     ylab = "Petal Length ",
     pch = 19,  
     col = "blue")

png(filename = "scatterplot_iris.png", width = 1920, height = 1080, res = 300)