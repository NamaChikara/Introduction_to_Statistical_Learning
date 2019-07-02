# 1) Set up environment.

library(MASS)
# install.packages("ISLR")
library(ISLR)

# 2) Investigate a data set.
fix(Boston)
names(Boston)

# 3) Investigate a simple linear model.
# Fit median house value = a * percentage of low income households + b
lstat_lm <- lm(medv~lstat, data = Boston)
# Examine the coefficients and their confidence intervals.
coef(lstat_lm)
confint(lstat_lm)
# Predict some responses on the confident intervals.
# -> The confidence and prediction intervals are centered around the same points,
#    but the prediction interval is significantly wider.
predict(lstat_lm, data.frame(lstat = c(5, 10, 15)), interval = "confidence")
predict(lstat_lm, data.frame(lstat = c(5, 10, 15)), interval = "prediction")

# Plot lstat against medv and include the linear model.
# a) using base R
plot(Boston$lstat, Boston$medv)
abline(lstat_lm)
# b) using ggplot2
ggplot(data = Boston, aes(x = lstat, y = medv)) +
  geom_point() +
  stat_smooth(method = "lm")

# Examine the residuals to gather additional evidence of nonlinearity.
# -> First, a residuals vs fits plot -- a scatter plot of residuals on the y
#    axis and fitted values on the x. If the model is a good fit, the residuals
#    should (i) be uncorrelated with the predicted value
#          (ii) no outliers
plot(predict(lstat_lm), residuals(lstat_lm))
abline(a = 0, b = 0)

ggplot(
  data.frame(prediction = lstat_lm$fitted.values,
             residuals = lstat_lm$residuals),
  aes(x = prediction, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0)
# -> Seconds, a studentized residuals vs fits plot.  Studentized residuals
#    are calculated by standardizing deleted residuals.  The deleted residual, di,
#    is defined as di := yi - hat(yi) where yi is the observed response of the ith
#    observation and hat(yi) is the predicted response if the ith observation is
#    left out of the model. A studentized residual value of >=3 is considered
#    an outlier.
plot(predict(lstat_lm), rstudent(lstat_lm))

# 4) Investigate a linear model with multiple predictor variables.

# Fit medv = a_1 * lstat + a_2 * age + b
multi_lm <- lm(medv ~ lstat + age, data = Boston)
# The R-squared value does not improve much after adding age as a predictor.
summary(multi_lm)$r.squared - summary(lstat_lm)$r.squared

# Fit medv_i = sum(a_i * x_i) + b
all_lm <- lm(medv ~ ., data = Boston)
summary(all_lm)

# Notice that age has the highest p value, so remove it from the model.
most_lm <- lm(medv ~ . - age, data = Boston)
summary(most_lm)

# 5) Investigate a linear model with interaction terms.

# Note: The syntax u:v tells R to include an interaction term between u and v.
#       The syntax u*v tells R to include u, v, and uv as predictors.
#       i.e. u * v = u + v + u:v.
interact_lm <- lm(medv ~ lstat * age, data = Boston)
summary(interact_lm)

# 6) Investigate a linear model with non-linear transformations of the predictors.

# Note: The syntax I(u^2) represents the predictor u^2; the I() wrapping is 
#       needed since the caret has a special meaning in a formula.
nonlin_lm <- lm(medv ~ lstat + I(lstat ^ 2), data = Boston)
summary(nonlin_lm)
# R squared improves significantly.
summary(nonlin_lm)$r.squared - summary(lstat_lm)$r.squared
# Fit looks better, too.
result <- data.frame(a = Boston$lstat,
                     b = predict(nonlin_lm))
actual <- data.frame(a = Boston$lstat,
                     b = Boston$medv)
ggplot(result, aes(x = a, y = b)) + 
  geom_point() +
  geom_point(data = actual, colour = "red") 
# The residuals have a more favorable distribution than the simple linear case.
par(mfrow = c(1,2))
plot(predict(nonlin_lm), residuals(nonlin_lm))
plot(predict(lstat_lm), residuals(lstat_lm))
par(mfrow = c(1,1))
# Analysis of variance, anova(), performs a hypothesis test: the null hypothesis
# is that the two models fit the data equally well. The p value is near zero, 
# providing evidence that the model containing lstat and lstat^2 is superior.
anova(lstat_lm, nonlin_lm)
# Check the F-statistic for adding subsequent terms to determine the point at
# which the null hypothesis is accepted. It looks like 5 terms is the appropriate
# amount to add.
p <- 0
for (i in seq(1, 10)) {
  first <- lm(medv ~ poly(lstat, i), data = Boston)
  second <- lm(medv ~ poly(lstat, i + 1), data = Boston)
  temp <- anova(first, second)
  first_data <- data.frame(lstat = Boston$lstat,
                           medv = predict(first))
  second_data <- data.frame(lstat = Boston$lstat,
                            medv = predict(second))
  p <- ggplot(Boston, aes(x = lstat, y = medv)) +
    geom_point() +
    geom_point(data = first_data, color = "red") + 
    geom_point(data = second_data, color = "blue") + 
    labs(title = paste(i, "order vs.", i + 1, " : ", temp$`Pr(>F)`[2]))
  print(p)
  print(paste(i, "order vs.", i + 1, " : ", temp$`Pr(>F)`[2]))
}





