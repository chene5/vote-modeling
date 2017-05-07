### Commands for tuning models for predicting vote turnout for the 2008 presidential election.
library(caret)

### Setup the dataset.
source("setup.R")
# Check out the df
names(g2012.imputed.data)
dim(g2012.imputed.data)

### Load the tuning function source file.
source("tuning_function.R")

# Tune models for full dataset.
sink("tuning_output.txt")
model.tuning(g2012.imputed.data, "VOTE08")
sink()

# Tune models for Black participants only.
sink("tuning_output_black.txt")
model.tuning(g2012.imputed.black, "VOTE08")
sink()

### Look at the output to decide what's the best model. Then build it.
## For example, this is an SVM Radial model (with some placeholder parameters).
# Use 10 repeat, 10-fold cv.
# Use downsampling to balance classes.
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           sampling = "down")
# Some placeholder parameters.
svmRadialGrid = expand.grid(C = c(16), sigma = c(.001))
# Train the model.
g2012.svmRadial <- train(VOTE08 ~ .,
                         data = g2012.imputed.data,
                         method = "svmRadial", 
                         trControl = fitControl,
                         preProc = c("center", "scale"),
                         tuneGrid = svmRadialGrid)
# Just use Accuracy as the metric. Could use ROC, etc.
g2012.svmRadial.best = best(g2012.svmRadial$results, metric = "Accuracy", maximize = TRUE)
