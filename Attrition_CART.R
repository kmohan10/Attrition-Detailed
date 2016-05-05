setwd("C:/Recovered Files/Krishna/PGPBA/Data Mining")
library(mice)
library(ROSE)
library(popbio)
library(partykit)
library(rpart)


attrition = read.csv("HR_Employee_Attrition_Data.csv")

# Rearrange the columns so that the dependent variable 'Attrition' is column #1
d = data.frame(attrition[,2],attrition[,1], attrition[,3:35])
colnames(d)[2] = "Age"
colnames(d)[1] = "Attrition"

attrition = d

# Compare some variables with the Dependent variable
head(attrition[,c(2:10,1)])

# Profile the dependent variable
attrition.ratio = table(attrition$Attrition)
attrition.ratio

attrition.prop = prop.table(attrition.ratio)
attrition.prop   #16% of the data has Attrition = "Yes"

# Identify columns with missing data
missing.info = md.pattern(attrition)
last.row <- nrow(missing.info)
missing.info[last.row,]   # None of the columns have any missing data
last.col = ncol(missing.info)
missing.info[,last.col]

# Split data into Train and Test
library(caret)
train = createDataPartition(attrition$Attrition, list=FALSE, times=1, p=0.7)

train.data = attrition[train,]
test.data = attrition[-train,]

#Making sure teh partition is right
prop.table((table(attrition$Attrition)))
prop.table((table(train.data$Attrition)))
prop.table((table(test.data$Attrition)))


# Build CART model using train.data
cart.model.1 = rpart(Attrition ~., data = train.data, method = "class", control = rpart.control(minsplit = 60, minbucket = 30, depth=6))
plot(as.party(cart.model.1))
print(cart.model.1)

summary(cart.model.1)

# To determine if the tree is appropriate or if some of the branches need to be subjected to pruning, we
# check on the cptable

printcp(cart.model.1)
plotcp(cart.model.1)
opt = which.min(cart.model.1$cptable[,"xerror"])
opt

  ## The model had 7 splits.  Looking at cross validated prediction error (xerror), the lowest value (0.87651) occurs at nsplit = 5.
  ## In other words, the model is suggesting that pruning is required.

  ##Variable importance indicates that the most important variables are MonthlyIncome, OverTime, TotalWorkingYears,
  ## MaritalStatus, JobRole, Department, StockOptionLevel and DailyRate.

# Pruning the tree to lease xsell error
cp<-cart.model.1$cptable [opt,"CP"]
cart.model.1.pruned <-prune(cart.model.1,cp=cp)
plot(as.party(cart.model.1.pruned))

print(cart.model.1.pruned)

printcp(cart.model.1.pruned)


# Predict on Training dataset using cart.model.1
predict_cart_train = predict(cart.model.1, newdata=train.data, type="class")

# Let us compare the predictions to compare them to actual outcome
table(train.data$Attrition, predict_cart_train)

# Predict on Training dataset using cart.model.1.pruned
predict_cart_train_pruned = predict(cart.model.1.pruned, newdata=train.data, type="class")

# Let us compare the predictions to compare them to actual outcome
table(train.data$Attrition, predict_cart_train_pruned)

## Although the pruned model gave a better overall accuracy, let us compare False Negatives. The pruned model
## predicted higher number as non-attrition which turned out to be attrition.  Therefore, the non-pruned is
## a better prediction model.

# Predict on Test dataset using cart.model.1
predict_cart_test = predict(cart.model.1, newdata=test.data, type="class")

# Let us compare the predictions to actual outcome
table(test.data$Attrition, predict_cart_test)

# Accuracy:
(723 + 31)/nrow(test.data)

# Sensitivity:
31/(111+31)

# Specificity:
16/(723+16)


# Build CART model using train.data
cart.model.2 = rpart(Attrition ~MonthlyIncome+OverTime+TotalWorkingYears+MaritalStatus+JobRole+
                       Department+StockOptionLevel+DailyRate, data = train.data, method = "class", control = rpart.control(minsplit = 60, minbucket = 30, depth=6))
plot(as.party(cart.model.2))
print(cart.model.2)

summary(cart.model.2)

# To determine if the tree is appropriate or if some of the branches need to be subjected to pruning, we
# check on the cptable

printcp(cart.model.1)
plotcp(cart.model.2)
opt = which.min(cart.model.2$cptable[,"xerror"])
opt

cp.pruned<-cart.model.2$cptable [opt,"CP"]
cart.model.2.pruned <-prune(cart.model.2,cp=cp.pruned)
plot(as.party(cart.model.2.pruned))

print(cart.model.2.pruned)

printcp(cart.model.2.pruned)


# Predict on Training dataset using cart.model.2
predict_cart_test_pruned = predict(cart.model.2, newdata=test.data, type="class")

# Let us compare the predictions to compare them to actual outcome
table(test.data$Attrition, predict_cart_test_pruned)

# Accuracy:
(717 + 34)/nrow(test.data)

# Sensitivity:
34/(108+34)

# Specificity:
22/(717+22)

