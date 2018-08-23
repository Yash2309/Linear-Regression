# Dataset
dataset_cars = as.data.frame(cars)

# Correlation
cor(dataset_cars$speed, dataset_cars$dist)

# Graphical Analysis
# 1. Scatter Plot
scatter.smooth(x = dataset_cars$speed, y = dataset_cars$dist, main = "Dist ~ Speed")

# 2. Density plot
par(mfrow = c(1, 2))
library(e1071)
plot(density(dataset_cars$dist), main = "Distance", 
     round(e1071::skewness(dataset_cars$dist), 2))
plot(density(dataset_cars$speed), main = "Speed", 
     round(e1071::skewness(dataset_cars$speed), 2))

# Splitting of dataset
library(caTools)
set.seed(1234)
split = sample.split(dataset_cars$speed, SplitRatio = 0.8)
training_cars = subset(dataset_cars, split == TRUE)
test_cars = subset(dataset_cars, split == FALSE)

# Applying the linear model
regressor_cars = lm(formula = speed ~ dist, data = training_cars)
print(regressor_cars)

y_pred_cars = predict(regressor_cars, newdata = test_cars)
round(y_pred_cars, 0)
summary(regressor_cars)