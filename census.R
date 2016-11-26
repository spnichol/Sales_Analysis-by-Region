

library(stringr)
library(corrplot)
library(caTools)
library(car)
library(pscl)
censusData <- read.csv("adult.csv", strip.white=TRUE)



censusData$workclass <- gsub("?", NA, censusData$workclass, fixed=TRUE)
censusData$workclass <- as.factor(censusData$workclass)

ageMean <- mean(censusData$age)
ageSD <- sd(censusData$age)
z <- (censusData$age - ageMean)/ageSD
z3 <- subset(censusData, z >= 3)
z2 <- subset(censusData, z <2)
z1 <- subset(censusData, z <1)

hist(censusData$age)
par(mfrow=c(4,1)) 
hist((censusData$age), col="blue",border="white")
hist((z1$age), col="blue",border="white")
hist((z2$age), col="blue",border="white")
hist((z3$age), col="blue",border="white")
plot(censusData$workclass)

censusData$income <- NULL


for (i in 1:nrow(censusData)) {
  if (censusData$salary[i] == "<=50K") {
    censusData$income[i] <- 0
  }
  else {
    censusData$income[i]<- 1
  }
}

censusData$income <- factor(censusData$income)



censusData$relationship <- NULL
for (i in 1:nrow(censusData)) {
  if (censusData$marital.status[i] == "Married-spouse-absent" ||censusData$marital.status[i] == "Married-civ-spouse" || censusData$marital.status[i] == "Married-AF-spouse") {
    censusData$relationship[i] <- 1
  }
  else {
    censusData$relationship[i] <- 0
  }
}





censusData["newSex"] <- NULL
for (i in 1:nrow(censusData)) {
  if (censusData$sex[i] == "Male") {
    censusData$newSex[i] <- 1
    
  }
  else  {
    censusData$newSex[i] <- 0
    
  }
 
}


censusData["capitalTotal"] <- censusData$capital.gain +  censusData$capital.loss

corrDF <- subset(censusData, select=c("age", "education.number", "hours.per.week", "income", "newSex", "relationship", "capitalTotal"))

 
 
as.factor(censusData$occupation)
set.seed(101)
sample <- sample.split(censusData, SplitRatio=.75) 
train = subset(censusData, sample == TRUE)
test = subset(censusData, sample == FALSE)
train <- na.omit(train)
test <- na.omit(test)
logitmodel <- glm(income ~ education.number + age + relationship + capitalTotal +hours.per.week + newSex, data=train, family = "binomial")
summary(logitmodel)
vif(logitmodel)


fitted.results <- predict(logitmodel, newdata=subset(test,select=c("age", "hours.per.week", "education.number", "income", "newSex", "relationship", "capitalTotal")), type='response')
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)
misClasificError <- mean(fitted.results != test$income)
print(paste('Accuracy', 1-misClasificError))





nullmod <- glm(censusData$income~1, family="binomial")
mcfaddenR2 <- 1-logLik(logitmodel)/logLik(nullmod)
mcfaddenR2
summary(mcfaddenR2)
library(caret)
confusionMatrix(fitted.results, test$income)
library(ROCR)
p <- fitted.results
pr <- prediction(p, test$income)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

