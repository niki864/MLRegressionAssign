library(ggplot2)
concreteds <-read.csv("concrete.csv", header = TRUE)
cor(concreteds)
library(car)
#Convert to data frame
dfconc <- as.data.frame(concreteds[1:11])
cor(dfconc)
#Plot Scatterplot for the data frame
scatterplotMatrix(dfconc, spread = FALSE, lty.smooth=2, main="Scatter Plot Matrix")
#1 Model for Slump
fitconc1 <- lm(Slump ~ Water+SP+Slag+Coarse.Aggregate+Fine.Aggregate ,data = dfconc)
fitconc <- lm(Slump ~ Water+SP+Slag,data = dfconc)
summary(fitconc3b)
#Find the confidence intervals for the ranges - Diagnostics - Typical Approach
confint(fitconc)
par(mfrow=c(2,2))
plot(fitconc)
#Enhanced approach to diagnostics
qqPlot(fitconc, labels=dfconc$No, id.method="identify",simulate=TRUE, main="Q-Q Plot")
# Residual Plot
residPlot <- function(fit, nbreaks=10){
  z <- rstudent(fit)
  hist(z, breaks = nbreaks, freq = FALSE, xlab = "Studentized Residuals",main = "Distribution of Errors")
  rug(jitter(z),col = 'brown')
  curve(dnorm(x, mean = mean(z), sd = sd(z)),add = TRUE, col="blue",lwd=2)
  lines(density(z)$x,density(z)$y, col="red",lwd=2,lty=2)
  legend("topright",legend = c("Normal Curve","Kernel Density Curve"), lty = 1:2, col = c("blue","red"), cex = 0.7)
}
residPlot(fitconc)
#Analysis of peformance of the 2 models using ANOVA testing
anova(fitconc,fitconc1)
#Recommended to go with model 2 based on Anova Analysis
################
# 2 Model For Slump Flow
fitconc2 <- lm(Slump.Flow ~ Water+Coarse.Aggregate+Slag ,data = dfconc)
fitconc2b <- lm(Slump.Flow ~ Water+Coarse.Aggregate+Slag+Fly.Ash ,data = dfconc) 
summary(fitconc2)
#Find the confidence intervals for the ranges - Diagnostics - Typical Approach
confint(fitconc2)
par(mfrow=c(2,2))
plot(fitconc2)
residPlot(fitconc2b)
anova(fitconc2,fitconc2b)
#We conclude that it
###############
# 3 Model For X28.Day Compressive Strength
fitconc3 <- lm(X28.day.Compressive.Strength ~ Water+Fly.Ash+Cement+Slag ,data = dfconc)
fitconc3b <- lm(X28.day.Compressive.Strength ~ Water+Fly.Ash+Cement+Slag+SP ,data = dfconc)
summary(fitconc3)
#Find the confidence intervals for the ranges - Diagnostics - Typical Approach
confint(fitconc3)
par(mfrow=c(2,2))
plot(fitconc3)
residPlot(fitconc3)
anova(fitconc3,fitconc3b)
#####
