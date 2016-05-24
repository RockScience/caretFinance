#library(caret)
library(mlbench)
data(Sonar)
nrow(Sonar)
head(Sonar)
set.seed(107) 
inTrain = caret::createDataPartition(y = Sonar$Class, p = 0.75, list=FALSE)
#inTrain = createTimeSlices(y = Sonar$Class, p = 0.75, list=FALSE)

str(inTrain)

training = Sonar[inTrain,]
testing  = Sonar[-inTrain,]
nrow(training)
nrow(testing)

plsFit = caret::train(Class ~ ., 
                      data = training, 
                      method = "pls",  # partial least squares discriminant analysis (PLSDA)
                      tuneLength = 15, # here, this controls the number of parameter set (for pls, it will try 1:15)
                      preProc = c("center","scale"))


# resampling method is specified with trControl

ctrl = caret::trainControl(method = "repeatedcv", 
                           repeats = 3)

plsFit = caret::train(Class ~ ., 
                      data = training, 
                      method = "pls",  # partial least squares discriminant analysis (PLSDA)
                      tuneLength = 15, # here, this controls the number of parameter set (for pls, it will try 1:15)
                      trControl = ctrl,
                      preProc = c("center","scale"))

# measures of performance are given as argument of trainControl, and custom performance measures passed to train

ctrl = caret::trainControl(method = "repeatedcv", 
                           repeats = 3,
                           classProbs = TRUE,
                           #summaryFunction takes the observed and predicted values and estimate some measure of performance:
                           summaryFunction = caret::twoClassSummary) 

plsFit = caret::train(Class ~ ., 
                      data = training, 
                      method = "pls",    # partial least squares discriminant analysis (PLSDA)
                      tuneLength = 15,   # here, this controls the number of parameter set (for pls, it will try 1:15)
                      trControl = ctrl,
                      metric = "ROC",    # that's the custom performance measure
                      preProc = c("center","scale"))

# visualisation methods
caret::plot.train(plsFit)

# predict
plsClasses <- caret::predict.train(plsFit, newdata = testing)

plsProbs <-  caret::predict.train(plsFit, newdata = testing, type = "prob")
head(plsProbs)

caret::confusionMatrix(data = plsClasses, reference = testing$Class)


# regularized discriminant model
rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)
set.seed(123)
rdaFit = caret::train(Class ~ ., 
                      data = training, 
                      method = "rda", 
                      tuneGrid = rdaGrid, 
                      trControl = ctrl, 
                      metric = "ROC")

caret::plot.train(rdaFit)

# with 2 parameters
rdaGrid = expand.grid(gamma = (0:4)/4, lambda = c(3/4,1)) # or genStratMatrix
set.seed(123)
rdaFit = caret::train(Class ~ ., 
                      data = training, 
                      method = "rda", 
                      tuneGrid = rdaGrid, 
                      trControl = ctrl, 
                      metric = "ROC")
caret::plot.train(rdaFit, plotType = "scatter") # default
caret::plot.train(rdaFit, plotType = "level")
caret::plot.train(rdaFit, plotType = "line")