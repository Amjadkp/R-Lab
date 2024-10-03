data("mtcars")

cyl_am <- table(mtcars$cyl, mtcars$am)

#bar plot
barplot(cyl_am,
        beside = TRUE,  
        col = c("red", "blue", "green"), 
        legend = rownames(cyl_am), 
        xlab = "Transmission (0 = Automatic, 1 = Manual)",  
        ylab = "Num Cars",  # Y-axis label
        main = "Num Cylinders by Transmission Type",  
        args.legend = list(title = "Number of Cylinders"))