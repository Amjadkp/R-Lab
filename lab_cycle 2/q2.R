data("mtcars")

plot(mtcars$mpg,mtcars$disp,xlab = "MPG",ylab = "DISP",col = as.factor(mtcars$cyl),pch=19)
lines(loess.smooth(mtcars$mpg,mtcars$disp), col = "yellow", lwd =2)

legend("topright", legend = levels(as.factor(mtcars$cyl)),
       col = 1:3, pch = 19, title = "Cylinders")