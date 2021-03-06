library(car)
library(ggplot2)
library(plyr)
origds <- read.csv("Forest.csv",header = TRUE)
forestds <- read.csv("Forest.csv",header = TRUE)
forestds$Month <- mapvalues(forestds$Month,from=c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'),to=c(1,2,3,4,5,6,7,8,9,10,11,12))
forestds$Day <- mapvalues(forestds$Day,from=c('mon','tue','wed','thu','fri','sat','sun'),to=c(1,2,3,4,5,6,7))
forestds[,3:4] <- sapply(forestds[,3:4], as.numeric)
sapply(forestds, class)
forestds$Area<-log1p(forestds$Area)
cor(forestds)
forestds<-as.data.frame(scale(forestds))
scatterplotMatrix(forestds, spread = FALSE, lty.smooth=2, main = "Forest Fire Scatter Plot")
######Model 1 - STFWTI
model1 <- lm(Area~X+Y+Month+Day+FFMC+DMC+DC+ISI+Temp+RH+Wind+Rain,data=forestds)
summary(model1)
confint(model1)
par(mfrow=c(2,2))
plot(model4)
residPlot <- function(fit, nbreaks=10){
  z <- rstudent(fit)
  hist(z+1, breaks = nbreaks, freq = FALSE, xlab = "Studentized Residuals",main = "Distribution of Errors")
  rug(jitter(z),col = 'brown')
  curve(dlnorm(x, mean = mean(z), sd = sd(z)),add = TRUE, col="blue",lwd=2)
  lines(density(z)$x,density(z)$y, col="red",lwd=2,lty=2)
  legend("topright",legend = c("Logarithmic Curve","Kernel Density Curve"), lty = 1:2, col = c("blue","red"), cex = 0.7)
}
residPlot(model1)
#####Model 2 -STM
model2  <- lm(Area~X+Y+Month+Day+Temp+RH+Wind+Rain,data=forestds)
summary(model2)
confint(model2)
residPlot(model2)
anova(model2,model1)
#####Model 3 -FWI
model3 <- lm(Area~FFMC+DMC+DC+ISI,data=forestds)
summary(model3)
residPlot(model3)
confint(model3)
anova(model3,model1)
######Model 4 -M
model4 <- lm(Area~Temp+RH+Wind+Rain,data=forestds)
summary(model4)
residPlot(model4)
anova(model4,model1)
#####Conclusion use model 4 as it has the highest p value and hence we fail to reject the null hypothesis that it is not better than the default model
