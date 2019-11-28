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

install.packages("groupdata2")
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

library(dplyr)
library(tidyr)
data = as.data.frame(data) %>% 
  separate(age, into = paste("age", c('begin', 'end'), sep = ".")) %>%
  separate(tumor.size, into = paste("tumor.size", c('begin', 'end'), sep = ".")) %>%
  separate(inv.nodes, into = paste("inv.nodes", c('begin', 'end'), sep = "."))

sample = sample.split(data$Class, SplitRatio = .75)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)

rf <- randomForest(
  Class ~ .,
  data=train
)

pred = predict(rf, newdata=test[-1])

cm = table(test[,1], pred)
cm

fourfoldplot(cm, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")

accuracy = (cm[2,2] + cm[1,1]) / sum(cm)
precision = cm[2,2] / (cm[2,2] + cm[1,2])
recall = cm[2,2] / (cm[2,2] + cm[2,1])

cor_matrix<-cor(data)
head(round(cor_matrix,2))


https://sebastiansauer.github.io/ordering-bars/

barplot(prop.table(table(data$deg.malig)))

head(data)
