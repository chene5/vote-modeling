##### Tuning function.
# This function tunes several different models:
# SVM Linear, SVM Radial, SVM Polynomial, and Random Forest
#
# Arguments:
# data : dataset
# y.var : variable to be predicted
# n.number : the number of folds for the repeated k-fold cv
# n.repeats : the number of repeats for the repeated k-fold cv
# n.tuneLength : the tuning length for the caret tuning function
# holdout : flag for whether to hold out a validation set
# do.downsample : flag for whether to downsample to balance the predicted classes
#
library(caret)


model.tuning <- function (data, y.var, 
                             n.number = 10, n.repeats = 10, n.tuneLength = 10,
                             holdout = TRUE,
                             do.downsample = TRUE) {
  print(paste("Analyses for:", y.var))
  print(table(data[[y.var]]))
  cat("\n")

  if (holdout) {
    ### Set up training and testing sets.
    # Create training index.
    g2012.train.index <- createDataPartition(data[[y.var]], p = .8,
                                             list = FALSE,
                                             times = 1)
    g2012.training = data[g2012.train.index,]
    g2012.testing = data[-g2012.train.index,]
    print(table(g2012.training[y.var]))
  }
  else {
    # Don't use a test set for validation.
    # Just use the entire dataset.
    g2012.training = data
  }

  # Setup formula.
  y.formula = as.formula(paste(y.var, "~", "."))

  ### Set up the training options.
  # Use repeated cross-validation.
  if (do.downsample) {
    # Use downsampling to balance classes.
    fitControl <- trainControl(method = "repeatedcv",
                               number = n.number,
                               repeats = n.repeats,
                               verbose = TRUE,
                               sampling = "down")
  }
  else {
    fitControl <- trainControl(method = "repeatedcv",
                               number = n.number,
                               repeats = n.repeats,
                               verbose = TRUE)
  }


  #### SVM Linear
  print("SVM Linear")
  # Set up the parameter search space.
  svmLinearGrid = expand.grid(C = c(0.001, 0.01, 0.1, 1, 5, 10, 100))
  # Train model.
  g2012.svmLinear <- train(y.formula,
                                data = g2012.training,
                                method = "svmLinear",
                                trControl = fitControl,
                                preProc = c("center", "scale"),
                                # tuneLength = n.tuneLength
                                tuneGrid = svmLinearGrid)
  # Get information about the model.
  print(g2012.svmLinear)
  print(varImp(g2012.svmLinear))
  g2012.svmLinear.best = best(g2012.svmLinear$results, metric = "Accuracy", maximize = TRUE)
  print(g2012.svmLinear$results[g2012.svmLinear.best,])

  if (holdout) {
    # See how well the model does.
    g2012.svmLinear.pred = predict(g2012.svmLinear, newdata = g2012.testing)
    print(confusionMatrix(g2012.svmLinear.pred, g2012.testing[[y.var]]))
  }

  cat("\n")
  cat("\n")


  #### SVM Radial
  print("SVM Radial")
  # Set up the parameter search space. Currently not using it though.
  svmRadialGrid = expand.grid(C = c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128),
                              sigma = c(.0001, .001, .005, .01, .1))
  # Train model.
  g2012.svmRadial <- train(y.formula,
                                data = g2012.training,
                                method = "svmRadial", 
                                trControl = fitControl,
                                preProc = c("center", "scale"),
                                # tuneGrid = svmRadialGrid
                                tuneLength = n.tuneLength)
  # Get information about the model.
  print(g2012.svmRadial)
  print(varImp(g2012.svmRadial))
  g2012.svmRadial.best = best(g2012.svmRadial$results, metric = "Accuracy", maximize = TRUE)
  print(g2012.svmRadial$results[g2012.svmRadial.best,])
  
  if (holdout) {
    # See how well the model does.
    g2012.svmRadial.pred = predict(g2012.svmRadial, newdata = g2012.testing)
    print(confusionMatrix(g2012.svmRadial.pred, g2012.testing[[y.var]]))
  }

  cat("\n")
  cat("\n")
  
  
  #### SVM Poly
  print("SVM Poly")
  # Set up the parameter search space.
  svmPolyGrid <-  expand.grid(degree = c(1, 2, 3, 4), 
                              scale = c(.001, .01, .1),
                              C = c(.25, .5, 1, 2, 8, 16, 100))
  # Train model.
  g2012.svmPoly <- train(y.formula,
                                data = g2012.training,
                                method = "svmPoly", 
                                trControl = fitControl,
                                preProc = c("center", "scale"),
                                tuneGrid = svmPolyGrid)
  # Get information about the model.
  print(g2012.svmPoly)
  print(varImp(g2012.svmPoly))
  g2012.svmPoly.best = best(g2012.svmPoly$results, metric = "Accuracy", maximize = TRUE)
  print(g2012.svmPoly$results[g2012.svmPoly.best,])
  
  if (holdout) {
    # See how well the model does.
    g2012.svmPoly.pred = predict(g2012.svmPoly, newdata = g2012.testing)
    print(confusionMatrix(g2012.svmPoly.pred, g2012.testing[[y.var]]))
  }

  cat("\n")
  cat("\n")
  
  
  #### Random Forest
  print("Random forest")
  # Train model.
  g2012.rf <- train(y.formula, 
                    data = g2012.training,
                    method = "rf", 
                    trControl = fitControl,
                    tuneLength = n.tuneLength)
  # Get information about the model.
  print(g2012.rf)
  # Catch any errors that might occur. 
  # Caret doesn't work well with random forest regressions.
  tryCatch(print(varImp(g2012.rf)), error = function(e) print(e))
  tryCatch({g2012.rf.best = best(g2012.rf$results, metric = "Accuracy", maximize = TRUE)
            print(g2012.rf$results[g2012.rf.best,])}, 
           error = function(e) print(e))

  if (holdout) {
    # See how well the model does.
    g2012.rf.pred = predict(g2012.rf, newdata = g2012.testing)
    print(confusionMatrix(g2012.rf.pred, g2012.testing[[y.var]]))
  }
}
