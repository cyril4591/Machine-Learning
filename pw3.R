#1)
#importer le fichier csv
dataset <- read.csv('D:/EcoleESILVA4/MachineLearning/Social_Network_Ads.csv')

#2)
#Explore and Describe the dataset
str(dataset)
summary(dataset)
boxplot(dataset)

#3)
#split the dataset
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#4)
#Scale the input variables in both training set and test set. Do you know what is scaling? Explain it one sentence.
training_set[c(3,4)] = scale(training_set[c(3,4)])
test_set[c(3,4)] = scale(test_set[c(3,4)])
#we want the data to have the same range in order to see them clearly.

#5)
#fit a simple logistic regression model of Purchased in function of Age
logreg <- glm(Purchased ~ Age , family = binomial, data=training_set)


#6)
#the variable we want to predict (eather they bought the product or not) is something that we can relate to a binary variable. 
#So we can model it as a binomial law with a seccess 1 and failure 0.

#7)
#Write in a equation the model you obtained in question 5
#p(X)=e(β0+β1X)/(1+e(β0+β1X))

#8)
# Is the feature Age significant to the model?
summary(logreg)
#the pvalue is realy small so we can assume that Age is significant (close to 0)

#9)
# What is the value of AIC of the model


#10)
#Plot Purchased in function of Age and add the curve of the obtained logistic regression model
plot(training_set$Age,training_set$Purchased)
curve(predict(logreg, data.frame(Age=x), type="response"), add=TRUE)

library(ggplot2)
ggplot(training_set, aes(x=Age, y=Purchased)) + geom_point() +stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

#11)
#Fit a logistic regression model of purchasing the product in function of the age of a user and its salary.
logreg2 <- glm(Purchased ~Age + EstimatedSalary , family = binomial, data=training_set)


#12)
#Are the predictors significant to the new model?
summary(logreg2)

#13)
#Do we obtain a better model by adding the estimated salary

#14)
#On the test set, predict the probability of purchasing the product by the users using the obtained model
prob_logreg2 = predict(logreg2, newdata = test_set[c(3,4)], type="response")

#15)
#transform the predicted values to 0 or 1 (1 if >0.5)
pred_logreg2 = ifelse(prob_logreg2 > 0.5, 1,0)

#16)
y = table(test_set[,5], pred_logreg2)
y

#17)
#Calculate the accuracy, specificity, sensitivity and the precision of the model

#18)
#Plot the ROC curve and calculate AUC value
library(ROCR)
score <- prediction(pred_logreg2,test_set[,5])
performance(score,"auc")

plot(performance(score,"tpr","fpr"))
abline(0,1,lty=8)
