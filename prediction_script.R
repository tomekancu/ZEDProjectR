data = read.table('data/breast-cancer.data', sep=',', header = F, col.names = c('Class', 'age', 'menopause', 'tumor-size', 
                                                                                'inv-nodes', 'node-caps', 'deg-malig', 'breast', 'breast-quad' ,'irradiat'))

for(age_cat in levels(data$age)){
  data[,paste0(age_cat)]=ifelse(data$age == age_cat, TRUE, FALSE)
}
data <- subset(data, select = -c(age))

for(tumor_size_cat in levels(data$tumor.size)){
  data[,paste0(tumor_size_cat)]=ifelse(data$tumor.size == tumor_size_cat, TRUE, FALSE)
}
data <- subset(data, select = -c(tumor.size))

install.packages("caTools")
library(randomForest)
require(caTools)

shuffle_index <- sample(1:nrow(data))
head(shuffle_index)
data <- data[shuffle_index, ]
head(data)
apply(data, )
dim(data)

sapply(data, class)
colSums(is.na(data))

sample = sample.split(data$Class   , SplitRatio = .75)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)

rf <- randomForest(
  Class ~ .,
  data=train
)

pred = predict(rf, newdata=test[-14])

cm = table(test[,1], pred)
cm
