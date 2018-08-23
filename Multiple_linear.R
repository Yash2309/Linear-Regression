# Dataset
dataset_iris = as.data.frame(iris)

# Encoding categorical data
dataset_iris$Species = factor(dataset_iris$Species, 
                              levels = c('setosa', 'versicolor', 'virginica'), 
                              labels = c(1, 2, 3))
# Correlation
cor(dataset_iris[-5], dataset_iris[-5])

# Graphical Analysis
# 1. Scatter Plot
plot(dataset_iris)

# 2. Density plot
par(mfrow = c(1,2))
plot(density(dataset_iris$Sepal.Length), round(e1071::skewness(dataset_iris$Sepal.Length), 2),
     main = "Sepal Length")
plot(density(dataset_iris$Sepal.Width), round(e1071::skewness(dataset_iris$Sepal.Width), 2),
     main = "Sepal Width")
plot(density(dataset_iris$Petal.Length), round(e1071::skewness(dataset_iris$Petal.Length), 2),
     main = "Petal Length")
plot(density(dataset_iris$Petal.Width), round(e1071::skewness(dataset_iris$Petal.Width), 2),
     main = "Petal Width")

# Splitting of data
library(caTools)
set.seed(123)
split = sample.split(dataset_iris$Petal.Length, SplitRatio = 0.8)
training_iris = subset(dataset_iris, split == TRUE)
test_iris = subset(dataset_iris, split == FALSE)

# Applying model
regressor_iris = lm(formula = Petal.Length ~., data = training_iris)
print(regressor_iris)

# Prediction
y_pred_iris = predict(regressor_iris, newdata = test_iris)
round(y_pred_iris, 1)

summary(regressor_iris)