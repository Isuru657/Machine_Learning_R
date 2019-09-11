glass <- read.csv(file.choose())
##Inspecting the dataset
head(glass)
tail(glass)
str(glass)
glass$Type <- factor(glass$Type)
##Normalizing the dataset
norm <- function(x) {x-min(x)/max(x)-min(x)}
glass_dat <- norm(glass_dat)
glass_dat <- glass[, -10]
#Splitting the data into test and train data
ran <- sample(1:nrow(glass_dat), size=nrow(glass_dat)*0.7)
train_dat<- glass_dat[ran, ]
test_dat <- glass_dat[-ran, ]
#Building a KNN model
mod_dat <- knn(train = train_dat, test = test_dat, cl=glass[ran,10], k=1)
mean(mod_dat==glass[-ran,10], na.rm= TRUE)
#Checking for accuracy using a confusion matrix
tab <- table(mod_dat, glass[-ran,10])
print(tab)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
##Finding which variables helps classification the most
##Using forward step-by-step regression
nullmodel <- glm(glass$Type ~ 1, data = glass, family="binomial")
fullmodel <- glm(glass$Type ~., data=glass, family="binomial")
step_model <- step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = "forward")
step_prob <- predict(step_model, type = "response")
library(pROC)
#Finding accuracy of model using a ROC Curve
ROC <- roc(glass$Type, step_prob)
print(ROC)
plot(ROC, col="red")
auc(ROC)
##Making prediction
new_predict <- predict(step_model, list(Mg=4.3 , Al=1.15, Na=13.0), interval="predict", level=.95)
new_predict
