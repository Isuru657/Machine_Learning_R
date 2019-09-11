location <- read.csv(file="https://assets.datacamp.com/production/repositories/718/datasets/571628c39048df59c40c9dcfba146a2cf7a4a0e3/locations.csv")
str(location)
head(location)
tail(location)
is.na(crime)
##Basic EDA
location%>%
  count(location)%>%
  mutate(mean_diff=n-mean(n, na.rm= TRUE))%>%
  mutate(fill=as.factor(ifelse(mean_diff>0, 0, 1)))%>%
  ggplot(., aes(x=fct_reorder(location, mean_diff), y=mean_diff, fill=fill))+
  geom_col()+ coord_flip()
##The dude hangs out at Home the most

##Using caret
install.packages("caret")
library(caret)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(123)
ran <- sample(nrow(location), nrow(location)*0.7)
train <- location[ran, ]
test <- location[-ran, ]
xtrain <- train[, -7]
ytrain <- train$location
xtest<- test[,-7]
ytest <- test$location 
model <- train(xtrain, ytrain, trControl= train_control, method="nb" )
t <- table(predict(model$finalModel, xtest)$class, ytest)

##Using random forests
set.seed(365)
ran1 <- sample(nrow(location), nrow(location)*0.7)
train1 <- location[ran1, ]
test1 <- location[-ran1, ]
xtrain1 <- train1[, -7]
ytrain1 <- train1$location
xtest1 <- test1[,-7]
ytest1 <- test1$location
fmod <- train(xtrain1, ytrain1, trControl= train_control, method="rf" )
t1 <- table(predict(model$finalModel, xtest1)$class, ytest1)

##Checking accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(t)
accuracy(t1)

##First model seems to be better than second model. CV gives better accuracy even on test data.














