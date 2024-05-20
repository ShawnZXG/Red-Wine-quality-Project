#Project Code

library(readxl)
library("psych")
library(car)
library(stargazer)
library(randomForest)
library(tree)
redwine <- read_excel("redwine.xlsx")
names(redwine)

df_redwine <- data.frame(redwine)
stargazer(df_redwine,type= "text", title ="Table 1. Summary Statistics of the Variables", out="summary table.htm")

pairs (redwine[,-12],gap=0, col="black")
dim(redwine)


#Data Exploration

# Box Plots
par(mfrow=c(1,4))
boxplot(alcohol, col="deepskyblue",xlab="alcohol")
boxplot(chlorides, col="deepskyblue",xlab="chlorides")
boxplot(citric_acid, col="deepskyblue",xlab="citric_acid")
boxplot(density, col="deepskyblue",xlab="density")
boxplot(fixed_acidity, col="deepskyblue",xlab="fixed_acidity")
boxplot(free_sulfur_dioxide, col="deepskyblue",xlab="free_sulfur_dioxide")
boxplot(pH, col="deepskyblue",xlab="pH")
boxplot(quality, col="deepskyblue",xlab="quality")
boxplot(residual_sugar, col="deepskyblue",xlab="residual_sugar")
boxplot(sulphates, col="deepskyblue",xlab="sulphates")
boxplot(total_sulfur_dioxide, col="deepskyblue",xlab="total_sulfur_dioxide")
boxplot(volatile_acidity, col="deepskyblue",xlab="volatile_acidity")


#################################################################3

#Multiple Linear Regression
lm.fit <- lm(quality~., data=redwine)
summary(lm.fit)

#check for multicollinearity using variance inflation factor (VIF).
vif(lm.fit)
#most of the VIF values are less than 10. So, there is not a problematic amount of collinearity.


##Backward Selection for Multiple Linear Regressions
#Removing the variables with the highest p-value one at a time 
#until the remaining variables are all statistically significant at the 5% level

lm.fit1 =lm(quality~.-density,data=redwine)
summary(lm.fit1)

lm.fit2 =update(lm.fit1, ~.-fixed_acidity)
summary(lm.fit2)

lm.fit3 =update(lm.fit2, ~.-residual_sugar)
summary(lm.fit3)

lm.fit4 =update(lm.fit3, ~.-citric_acid)
summary(lm.fit4)

stargazer(lm.fit ,lm.fit1,lm.fit2,lm.fit3, lm.fit4, 
          title ="Table 2. Multiple Linear Regression Results", 
          type= "text", omit.stat=c("LL","f"),
          no.space=TRUE,out="lm results.htm",
          order=c("density","fixed_acidity","residual_sugar","citric_acid"))


##################################################################################################


##Tree-based Methods--Classification Tree

#Quality scores were categorized in Excel into 3 categories and saved in a new file called "redwine_recoded.xlsx"
#Low: (3, 4), Medium (5,6), High: (7,8)
redwine_recoded <- read_excel("redwine_recoded.xlsx")

#Classification Tree
redwine_recoded['quality_classified'] <- as.factor(redwine_recoded$quality_classified)
sapply(redwine_recoded,class)

#checking that quality_classified is converted to "factor"
class(redwine_recoded$quality_classified)

tree.redwine=tree(quality_classified~fixed_acidity+
                    volatile_acidity+citric_acid+residual_sugar+
                    chlorides+free_sulfur_dioxide+total_sulfur_dioxide+
                    density+pH+sulphates+alcohol,redwine_recoded)

prop.table(table(redwine_recoded$quality_classified))
par(mfrow=c(1,1))

plot(tree.redwine)
text(tree.redwine,pretty=0)
title(main ="Unpruned Classfication Tree") 
      

#Estimating test error
set.seed(2)
# Using a random sample of half of the observations from the data set as the training data set
train=sample(1:nrow(redwine_recoded), nrow(redwine_recoded)/2)
length(train)


attach (redwine_recoded)
#creating a test data set
redwine.test=redwine_recoded[-train,]
dim(redwine.test)

#Setting the DV using the indexes of the test data set
quality_classified.test= quality_classified[-train] 
length(quality_classified.test)

#classification tree built using the training data set
tree.redwine=tree(quality_classified~fixed_acidity+volatile_acidity+citric_acid+residual_sugar+chlorides+free_sulfur_dioxide+total_sulfur_dioxide+density+pH+sulphates+alcohol,redwine_recoded,subset=train)

#using the model to make predictions on the testing data set
tree.pred=predict(tree.redwine,redwine.test,type="class")
table(tree.pred,quality_classified.test)

# finding accuracy rate
mean(tree.pred==quality_classified.test)
# accuracy = 0.8225


##Using cross-validation to find the optimal tree size
set.seed(2)
cv.redwine=cv.tree(tree.redwine,FUN=prune.misclass)
names(cv.redwine)
cv.redwine
#tree with size 8 to give the lowest dev (cross-validation error rate)

# Plotting the error rate as a function of tree size
par(mfrow=c(1,1))
plot(cv.redwine$size,cv.redwine$dev,type="b")
title(main ="Error Rate as a Function of Tree Size") 

# Pruning the tree to obtain the 8-node tree
set.seed(2)
prune.redwine=prune.misclass(tree.redwine,best=8)
plot(prune.redwine)
text(prune.redwine,pretty=0)
title(main ="Pruned Classfication Tree") 

# Evaluating the accuracy rate of the 8-node tree
tree.pred=predict(prune.redwine,redwine.test,type="class")
table(tree.pred,quality_classified.test)
mean(tree.pred==quality_classified.test)
#the accuracy rate of the pruned tree improved from 0.8225 to 0.8288



##################################################################################################################


#Random Forest 

#Sampling half the data as training data set
train=sample(1:nrow(redwine_recoded), nrow(redwine_recoded)/2)

redwine.test=redwine_recoded[-train,]

#Setting the DV based on the test data set
quality_classified.test= quality_classified[-train] 
length(quality_classified.test)


rf.redwine <- randomForest(quality_classified~fixed_acidity+volatile_acidity
                           +citric_acid+residual_sugar+chlorides+
                             free_sulfur_dioxide+total_sulfur_dioxide+density+pH+sulphates+alcohol,
                            data = redwine_recoded,subset=train,mtry=11/3, importance=TRUE)
rf.redwine
importance(rf.redwine)
varImpPlot(rf.redwine)

##Using random forest model to make predictions on the testing data set
yhat.rf <- predict(rf.redwine,newdata=redwine.test)
# finding accuracy rate
mean(yhat.rf==quality_classified.test)

table(yhat.rf,quality_classified.test)
(51+643)/(51+1+32+4+50+19+643)
694/800
#The accuracy rate (0.8675) is improved from that of the pruned classification tree (0.82875) 

#############################################################

