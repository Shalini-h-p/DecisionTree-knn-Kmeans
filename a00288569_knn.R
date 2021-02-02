# 2 KNN Algorithm
# read the data
library(class)
library(gmodels)

htru <- as.data.frame(read.csv("data/htru.csv"));
htruC <- htru[9];
htru$HTRU <- NULL;
htru <- as.data.frame(scale(htru));
htru[9]<- htruC;
# table of proportions 
prop.table(table(htru$HTRU));

# create a random sample for training and test data
set.seed(3)
htru_rand <- htru[order(runif(17898)), ]
htru_train <- htru_rand[1:16897,]
htru_test  <- htru_rand[16898:17898,]
htru_train_labels <- htru_rand[1:16897, 9]
htru_test_labels <- htru_rand[16898:17898, 9]

# check the proportions in test and train data
prop.table(table(htru_train$HTRU))
prop.table(table(htru_test$HTRU))

# k=3
predictions <- knn(train = htru_train, test = htru_test, 
                   cl = htru_train_labels, k=3)

predictions <- factor(predictions, labels = c("Negative", "Positive"));
htru_test_labels <- factor(htru_test_labels, labels = c("Negative", "Positive"));

CrossTable(predictions, htru_test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted default', 'actual default'))
