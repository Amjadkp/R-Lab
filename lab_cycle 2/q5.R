#Create a box plot of the horsepower (hp) in the mtcars dataset. Use
#different shapes to represent the number of gears (gear). Add
#appropriate title, labels, and legend to the plot. Identify and label any
#outliers on the plot.

data("mtcars")
boxplot_hp<-boxplot(mtcars$hp~mtcars$gear,
                    main = "Horsepower by Number of Gears",
                    xlab = "Num of Gears",
                    ylab = "Horsepower",
                    col = "blue",
                    pch = 19)
legend("topright",legend = levels(as.factor(mtcars$gear)),pch=19,title = "num gears")

outliers <- boxplot.stats(mtcars$hp ~ as.factor(mtcars$gear))$out
outlier_indices <- which(mtcars$hp %in% outliers)
text(rep(1:3, each = length(outliers)), outliers, labels = outlier_indices, pos = 3, col = "red", cex = 0.8)