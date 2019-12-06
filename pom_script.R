# Load libs
library(dplyr)
library(tidyr)
library(mlbench)
library(caret)
# Load data
data <- read.table('data/breast-cancer.data', sep = ',', header = F,
                   col.names = c('Class', 'age', 'menopause', 'tumor-size',
                                 'inv-nodes', 'node-caps', 'deg-malig', 'breast',
                                 'breast-quad', 'irradiat'))
data <- as.data.frame(data) %>%
  separate(age, into = paste("age", c('begin', 'end'), sep = ".")) %>%
  separate(tumor.size, into = paste("tumor.size", c('begin', 'end'), sep = ".")) %>%
  separate(inv.nodes, into = paste("inv.nodes", c('begin', 'end'), sep = "."))

levels(data$Class) <- c("no.recurrence.events", "recurrence.events")
levels(data$node.caps) <- c("any", "no", "yes")
levels(data$breast.quad) <- c("any", "central", "left.low", "left.up", "right.low", "right.up")

data$age.begin <- as.numeric(data$age.begin)
data$age.end <- as.numeric(data$age.end)

data$tumor.size.begin <- as.numeric(data$tumor.size.begin)
data$tumor.size.end <- as.numeric(data$tumor.size.end)

data$inv.nodes.begin <- as.numeric(data$inv.nodes.begin)
data$inv.nodes.end <- as.numeric(data$inv.nodes.end)

head(data)
summary(data)
# Train and test sets
sizeOfClass <- round(nrow(filter(data, Class == 'recurrence.events')) * 0.7)

inTraining <- 1:sizeOfClass

recTrain <- filter(data, Class == 'recurrence.events')[inTraining,]
recTest <- filter(data, Class == 'recurrence.events')[-inTraining,]

noRecTrain <- filter(data, Class == 'no.recurrence.events')[inTraining,]
noRecTest <- filter(data, Class == 'no.recurrence.events')[-inTraining,]

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
# Classify
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     search = 'random',
                     allowParallel = TRUE)

rfGrid <- expand.grid(mtry = 1:5, ntree = c(10, 100, 1000))

set.seed(23)
fitTune <- train(Class ~ .,
                 data = training,
                 method = "rf",
                 metric = "Recall",
                 preProc = c("center", "scale"),
                 trControl = ctrl,
                 ntree = 1000)
fitTune
rfClasses2 <- predict(fitTune, newdata = testing)
confusionMatrix(data = rfClasses2, testing$Class)
