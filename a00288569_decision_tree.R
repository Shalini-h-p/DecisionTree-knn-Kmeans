# 1 Decision Tree

# read the data
library(C50)
library(gmodels)
nurseryData <- as.data.frame(read.csv("data/nursery.csv"))
summary(nurseryData)

# look at characteristics 
table(nurseryData$parents)
table(nurseryData$health)

# look at characteristics
summary(nurseryData$parents)
summary(nurseryData$result)

# training and test split

# create a random sample for training and test data
set.seed(1)
nurseryData_rand <- nurseryData[order(runif(12960)),]
nurseryData_train <- nurseryData_rand[1:10960,]
nurseryData_test  <- nurseryData_rand[10961:12960,]

# check the proportion of class variable
prop.table(table(nurseryData_train$result))
prop.table(table(nurseryData_test$result))

# Build the model
nurseryDataModel <- C5.0(result ~ ., data = nurseryData_train)
# display simple facts about the tree
nurseryDataModel
plot(nurseryDataModel)
# display detailed information about the tree
summary(nurseryDataModel)

# Evaluate

predictions <- predict(nurseryDataModel, nurseryData_test)

CrossTable(predictions, nurseryData_test$result,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted default', 'actual default'))



