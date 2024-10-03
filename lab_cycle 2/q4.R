#Create a histogram of the miles per gallon (mpg) in the mtcars dataset.
#Use different shades of blue to represent the frequency of each bin.
#Add appropriate title and labels to the plot. Calculate and display the
#mean and standard deviation of mpg on the plot
data("mtcars")

hist(mtcars$mpg,col = colorRampPalette(c("lightblue", "darkblue"))(10),
     xlab = "MPG",main = "Mile per Gallon ")

mean_mpg<-mean(mtcars$mpg)
std_mpg<-sd(mtcars$mpg)

text(max(mtcars$mpg) - 5, 10, paste("Mean =", round(mean_mpg, 2)), col = "green", pos = 4)
text(max(mtcars$mpg) - 5, 9, paste("SD =", round(std_mpg, 2)), col = "red", pos = 4)