setwd('C:/Users/EmmaE/OneDrive/Desktop/msba/ML2')

#part a 
#Generate a data set with p = 20 features, n = 1,000 observations, and an 
#associated quantitative response vector generated according to the model 
#Y=Y=??X+??, where ?? has some elements that are exactly equal to zero.

set.seed(1)
x <- matrix(rnorm(1000 * 20), 1000, 20)
b <- rnorm(20)
b[3] <- 0
b[4] <- 0
b[9] <- 0
b[19] <- 0
b[10] <- 0
eps <- rnorm(1000)
y <- x %*% b + eps

#uses normal distribution
#Not using a data set, rather using randomly generated data

#part b 
#Split your dataset into a training set containing 100 observations and a 
#test set containing 900 observations.

train <- sample(seq(1000), 100, replace = FALSE)
test <- (-train)
x.train <- x[train, ]
x.test <- x[test, ]
y.train <- y[train]
y.test <- y[test]

#part c
#Perform best subset selection on the training set, and 
#plot the training set MSE associated with the best model of each size.

data.train <- data.frame(y = y.train, x = x.train)
regfit.full <- regsubsets(y ~ ., data = data.train, nvmax = 20)
train.mat <- model.matrix(y ~ ., data = data.train, nvmax = 20)
trian_error <- c()
for (i in 1:20) {
  coefi <- coef(regfit.full, id = i)
  pred <- train.mat[, names(coefi)] %*% coefi
  trian_error[i] <- mean((pred - y.train)^2)
}
plot(trian_error, xlab = "Number of Features Used", ylab = "Training MSE", pch = 19, type = "b")

#From the textbook:
#regsubsets() performs best sub-regsubsets() set selection by identifying the 
#best model that contains a given number of predictors, where best is quantified 
#using RSS. 
#The syntax for regsubsets() is the same as for lm().

regfit_best <- regsubsets(x = x_train, y = y_train, nvmax = 20)

regfit_best_summary <- summary(regfit_best)

data_frame(MSE = regfit_best_summary$rss/900) %>%
  mutate(id = row_number()) %>%
  ggplot(aes(id, MSE)) +
  geom_line() + geom_point(type = 9) +
  xlab('Number of Features Used') +
  ggtitle('MSE on Training Set') +
  theme_tufte() +
  scale_x_continuous(breaks = 1:20)

data_frame(train_error = regfit_best_summary$rss/900, vars = 1:20) %>%
  spread(vars, train_error)

#Training MSE decreases as the amount of features increases.
#It has diminishing effects, but it still decreases.

#part d
#Plot the test set MSE associated with the best model of each size.

#In the loop, for each size i, we extract the coefficients from regfit.full 
#for the best model of that size, multiply them into the appropriate columns 
#of the test model matrix to form the predictions, and compute the test MSE.

data.test <- data.frame(y = y.test, x = x.test)
test.mat <- model.matrix(y ~ ., data = data.test, nvmax = 20)
test_error <- c()
for (i in 1:20) {
  coefi <- coef(regfit.full, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  test_error[i] <- mean((pred - y.test)^2)
}
plot(test_error, xlab = "Number of predictors", ylab = "Test MSE", pch = 19, type = "b")

#part e 
#Here we are using the which.min function to help us determine in which model size the test set takes on its minimum value. 
which.min(test_error)

#part f 
# We arbitrarily set 5 zeros in our coefficients, that means in about 25% of cases 
# the coefficient will be 0. 25% of 20 is 4. However, in the test MSE line graph, 
# we observe the MSE is smallest when 15 features are included, which means when
# 5 features are included, model performs the best on test set.


# part g
#Create a plot displaying ???(???pj=1(??j?????^rj)^2) for a range of values of r, where 
#??^rj is the jth coefficient estimate for the best model containing r coefficients. 
#Comment on what you observe. How does this compare to the test MSE plot from (d)?

#In simpler terms, compare the distance between the original slope and the slope 
#that our regression calculates.

val.errors <- c()
x_cols = colnames(x, do.NULL = FALSE, prefix = "x.")
for (i in 1:20) {
  coefi <- coef(regfit.full, id = i)
  val.errors[i] <- sqrt(sum((b[x_cols %in% names(coefi)] - coefi[names(coefi) %in% x_cols])^2))
}
plot(val.errors, xlab = "Number of coefficients", 
     ylab = "Error between estimated and true coefficients", pch = 19, type = "b")
# Observe that when only 5 features are included, it gives us the least error 
# between real coefficients and estimated coefficients.
