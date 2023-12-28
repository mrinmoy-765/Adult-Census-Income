data<- read.csv("D:/adult.csv",header = TRUE,sep = ",")
data

str(data)
summary(data)

library(dplyr)

data <- data[ , -which(names(data) %in% c("education.num", "fnlwgt"))] 
data


min_age <- min(data$age)
max_age <- max(data$age)


cat("Minimum Age:", min_age, "\n")
cat("Maximum Age:", max_age, "\n")

install.packages("dplyr")
library(dplyr)
data$age <- case_when(
  data$age <= 17 ~ "Adolescence",
  data$age <= 24 ~ "Young Adult",
  data$age <= 64 ~ "Adult",
  data$age >= 65 ~ "Senior")
View(data) 


filtered_data <- data[is.finite(data$hours.per.week), ]
min_hour <- min(filtered_data$hours.per.week)
max_hour <- max(filtered_data$hours.per.week)
cat("Minimum Hour:", min_hour, "\n")
cat("Maximum Hour:", max_hour, "\n")


data$hours.per.week <- case_when(
  data$hours.per.week <= 20 ~ "Medium",
  data$hours.per.week <= 40 ~ "Good",
  data$hours.per.week <= 60 ~ "active",
  data$hours.per.week <= 99 ~ "Hard worker")


table(data$hours.per.week)


min_cgain <- min(data$capital.gain)
max_cgain <- max(data$capital.gain)
cat("Minimum Capital gain:", min_cgain, "\n")
cat("Maximum Capital gain:", max_cgain, "\n")


data$capital.gain <- case_when(
  data$capital.gain <= 0 ~ "No Capital Gain",
  data$capital.gain <= 5000 ~ "Low",
  data$capital.gain <= 20000 ~ "Moderate",
  data$capital.gain <= 50000 ~ "High",
  data$capital.gain <= 99999 ~ "very High")


table(data$capital.gain)

min_closs <- min(data$capital.loss)
max_closs <- max(data$capital.loss)
cat("Minimum Capital loss:", min_closs, "\n")
cat("Maximum Capital loss:", max_closs, "\n")



data$capital.loss <- case_when(
  data$capital.loss <= 0 ~ "No Capital Loss",
  data$capital.loss <= 1000 ~ "Low",
  data$capital.loss <= 2000 ~ "Moderate",
  data$capital.loss <= 3000 ~ "High",
  data$capital.loss <= 4356 ~ "very High",)


table(data$capital.loss)
View(data)




data[data == '?'] <- NA
data
colSums(is.na(data))

removedata<- na.omit(data)
removedata
mydata<-removedata
colSums(is.na(mydata))
View(mydata)

total_rows <- nrow(mydata)
total_columns <- ncol(mydata)
cat("Total Rows:", total_rows, "\n")
cat("Total Columns:", total_columns, "\n")

target <- as.factor(data$income)
features <- data[, -which(names(data) == 'income')]
chi_squared <- vector("numeric", length = ncol(features))
for (i in 1:ncol(features)) {
  contingency_table <- table(features[, i], target)
  chi_squared[i] <- chisq.test(contingency_table)$statistic
}
sorted_features <- names(features)[order(chi_squared, decreasing = TRUE)]
cat("Chi-Squared values for each feature:\n")
print(chi_squared)
cat("\nSorted features based on Chi-Squared values:\n")
print(sorted_features)

install.packages("tidyverse")
library(tidyverse)
library(caret)
library(ISLR)
install.packages("caret")
install.packages("ISLR")

glimpse(mydata)
table(mydata$income)

install.packages("smotefamily")
install.packages("ROSE")

library(ROSE)
library(smotefamily)

over=ovun.sample(income~.,data = mydata,method="over")
over=over$data
table(over$income)



over$income <- as.factor(over$income)
over$workclass <- as.factor(over$workclass)
over$education <- as.factor(over$education)
over$marital.status <- as.factor(over$marital.status)
over$occupation <- as.factor(over$occupation)
over$relationship <- as.factor(over$relationship)
over$race <- as.factor(over$race)
over$sex <- as.factor(over$sex)
over$capital.gain <- as.factor(over$capital.gain)
over$capital.loss <- as.factor(over$capital.loss)
over$hours.per.week <- as.factor(over$hours.per.week)
over$native.country <- as.factor(over$native.country)

set.seed(123)
train_control <- trainControl(method = "cv", 
                              number = 10)
model <- train(income~., data = over, 
               trControl = train_control, 
               method = "naive_bayes")

print(model)

install.packages(c("e1071", "caTools", "caret"))
library(e1071)
library(caTools)
library(caret)

split <- sample.split(over, SplitRatio = 0.7)
train_cl <- subset(over, split == "TRUE")
test_cl <- subset(over, split == "FALSE")
set.seed(120) 
classifier_cl <- naiveBayes(income ~ ., data = train_cl)
classifier_cl
y_pred <- predict(classifier_cl, newdata = test_cl)
cm <- table(test_cl$income, y_pred)
cm
confusionMatrix(cm)
recall <- cm[2, 2] / sum(cm[2, ])  
precision <- cm[2, 2] / sum(cm[, 2])  
f_measure <- 2 * (precision * recall) / (precision + recall)  # F1 Score
cat("Recall:", recall, "\n")
cat("Precision:", precision, "\n")
cat("F-measure:", f_measure, "\n")


