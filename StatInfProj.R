## Statistical inference project
## Mark Sucato

library(knitr)
library(car)
library(datasets)
lambda <- 0.2
n <- 40
results <- NULL
reps <- 1000
set.seed(1234)

for(i in 1:reps) {
	results[i] <- mean(rexp(n, lambda))
}
h <- hist(results, breaks=20, main = "Figure 1: Distribution of 1000 Simulated Exponential Sample Means
	 (n=40)", xlab = "Means of Exponential Samples (n=40)", col="firebrick")
xfit <- seq(min(results), max(results), length=length(results))
yfit <- dnorm(xfit, mean=mean(results), sd=sd(results))
yfit <- yfit*diff(h$mids[1:2])*length(results)
lines(xfit, yfit, col="blue", lwd=2)

mu <- 1/lambda           #theoretical exponential mean
xBar <- mean(results)
Value <- c(mu, xBar)
Means <- c("Population", "Sample")
kable(cbind(Means, Value), align= "lc", caption = "Table 1: Comparison of Means", digits=3)

varExp <- 1/lambda^2     #theoretical exponential variance
varBar <- varExp/n       #sample population theoretical
varActual<- var(results) #sample population actual
Value <- c(varBar, varActual)
Variance <- c("Theoretical Sample", "Actual Sample")
kable(cbind(Variance, Value), align= "lc", caption = "Table 1: Comparison of Means", digits=3)

sw <- shapiro.test(results)  #Shapiro-Wilks test of normality, reject null hypo of normality
ks <- ks.test(results, pnorm, mean(results), sd(results))  # Kolmogorov-Smirnov test of normality
pValue <- c(sw$p.value, ks$p.value)
Test <- c("Shapiro-Wilks", "Kolmogorov-Smirnov")
kable(cbind(Test, pValue), digits=3)
qqPlot(results, ylab = "Simulated Distribution", xlab = "Normal Distribution",
	main = "QQ Plot for 1000 Simulated Exponential Sample Means",
	id = FALSE)

hist(mean(rexp(1000)))
mns = NULL
for (i in 1 : 1000) mns = c(mns, mean(runif(40)))
hist(mns)

for(i in 1:reps*40) {
	results[i] <- mean(rexp(1, lambda))
}
h <- hist(results, breaks=20, main = "Distribution of 1000 Simulated Exponential Sample Means
	 (n=40)", xlab = "Means of Exponential Samples (n=40)", col="firebrick")

## Part 2

data <- ToothGrowth




