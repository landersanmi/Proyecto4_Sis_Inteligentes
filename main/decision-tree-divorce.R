# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Install required packages
library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)

# Read data from CSV
filename = "../data/divorce.csv"
data <- read.csv(file = filename, sep =";", header = TRUE)
# Convert columns to factors
index <- 1:ncol(data)
data[ , index] <- lapply(data[ , index], as.factor)

print(data)
best_tree <- list()
# Percentaje of training examples
training_p <- 0.8
for (i in 1:5) {
  # Generate data partition 80% training / 20% test. The result is a vector with the indexes 
  # of the examples that will be used for the training of the model.
  training_indexes <- createDataPartition(y = data$Class, p = training_p, list = FALSE)
  
  # Split training and test data
  training_data <- data[training_indexes, ]  # Extract training data using training_indexes
  test_data     <- data[-training_indexes, ] # Extract data with the indexes not included in training_indexes 
  
  # Create Linear Model using training data. Formula = all the columns except Class
  rpart.control(minsplit= 120 , minbucket=120, max_depth=54)
  model <- rpart(formula = Class ~., data = training_data)
  
  # Make the prediction using the model and test data
  prediction <- predict(model, test_data, type = "class")
  
  # Calculate accuracy using Confusion Matrix
  prediction_results <- table(test_data$Class, prediction)
  matrix <- confusionMatrix(prediction_results)
  accuracy <- matrix$overall[1]
  attrs <- names(model$variable.importance)
  
  # Compare if this is a better tree
  if(length(best_tree)==0){
    print("Vacio")
    best_tree$accuracy = accuracy
    best_tree$model = model
  }else if(accuracy > best_tree$accuracy){
    print("Mejor")
    best_tree$accuracy = accuracy
    print(accuracy)
    best_tree$model = model
  }
  
  print(paste0("Accuracy = ", round(accuracy, digits = 4)), quote = FALSE)
  
  for (i in 1:length(attrs)) {
    print(paste0("  ", attrs[i]), quote = FALSE)
  }
  
  # Print the rules that represent the Tree
  rpart.rules(model, extra = 4, cover = TRUE)
}

# Plot tree (this method is slow, wait until pot is completed)
rpart.plot(best_tree$model, 
           type = 2,
           extra = 101,
           fallen.leaves = FALSE,
           main = "Prescription", 
           sub = paste0("Acccuracy = ", round(accuracy, digits = 4)))
  
