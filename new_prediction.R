library(dplyr)
library(tidyr)
library(mlbench)
library(caret)

data = read.table('data/breast-cancer.data', sep=',', header = F, col.names = c('Class', 'age', 'menopause', 'tumor-size', 
                                                                                'inv-nodes', 'node-caps', 'deg-malig', 'breast', 'breast-quad' ,'irradiat'))
data = as.data.frame(data) %>% 
  separate(age, into = paste("age", c('begin', 'end'), sep = ".")) %>%
  separate(tumor.size, into = paste("tumor.size", c('begin', 'end'), sep = ".")) %>%
  separate(inv.nodes, into = paste("inv.nodes", c('begin', 'end'), sep = "."))
head(data)
summary(data)

sizeOfClass <-  round(count(filter(data, Class=='recurrence-events')) * 0.7)[1, 'n']

inTraining <- 1:sizeOfClass

recTrain <- filter(data, Class=='recurrence-events')[inTraining, ]
recTest <- filter(data, Class=='recurrence-events')[-inTraining, ]

noRecTrain <- filter(data, Class=='no-recurrence-events')[inTraining, ]
noRecTest <- filter(data, Class=='no-recurrence-events')[-inTraining, ]

training <- rbind(recTrain, noRecTrain)
testing <- rbind(recTest, noRecTest)


# set.seed(23)
#inTraining <- 
#   createDataPartition(
    # attribute stratification
#    y = data$Class,
    # seperating class labels
#    p = .75,
    # using hold out scheme
#    list = FALSE)

#training <- data[ inTraining,]
#testing  <- data[-inTraining,]
#training = upsample(training, cat_col="Class")

summary(training$Class)
summary(testing$Class)

ctrl <- trainControl(
  # Repeated cross-validation
  method = "repeatedcv",
  # The number of divisions
  number = 2,
  # The number of repetitions
  repeats = 5)

set.seed(23)
fit <- train(Class ~ .,
             data = training,
             method = "rf",
             trControl = ctrl,
             # Parameter for the training algorithm
             ntree = 10)

fit

rfClasses <- predict(fit, newdata = testing)
confusionMatrix(data = rfClasses, testing$Class)


rfGrid <- expand.grid(mtry = 10:30)
gridCtrl <- trainControl(
  method = "repeatedcv",
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  number = 2,
  repeats = 5)

set.seed(23)
fitTune <- train(Class ~ .,
                 data = training,
                 method = "rf",
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 trControl = gridCtrl,
                 tuneGrid = rfGrid,
                 ntree = 30)

fitTune