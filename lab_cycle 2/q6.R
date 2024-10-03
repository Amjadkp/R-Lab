
data("mtcars")

colors <- rainbow(length(unique(mtcars$carb)))  

plot(mtcars$disp, mtcars$wt,
     col = colors[as.factor(mtcars$carb)],  
     pch = 19,  
     xlab = "Displacement", 
     ylab = "Weight ",
     main = "Displacement vs Weight by Num Carburetors")

lines(loess.smooth(mtcars$disp, mtcars$wt), col = "black", lwd = 2)

legend("topright", 
       legend = paste("Carburetors:", unique(mtcars$carb)),
       col = colors, 
       pch = 19, 
       title = "Number of Carburetors")