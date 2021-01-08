#Reading both files
titanic.train<- read.csv(file="train.csv", stringsAsFactors = FALSE, header= TRUE)
titanic.test<- read.csv(file="test.csv", stringsAsFactors = FALSE, header= TRUE)

#Before combining, need a way to know 
#which row is from which file
titanic.train$IsTrainSet<- TRUE
titanic.test$IsTrainSet<- FALSE

#test.csv has one column less
#Should have same no. of columns and same names
#Adding 
titanic.test$Survived <- NA

#Combining
titanic.full<- rbind(titanic.train, titanic.test)

#Cleaning Embarked(shifting missing values to S)
titanic.full[titanic.full$Embarked=='', "Embarked"]<- 'S'

#Cleaning Age
age.median<- median(titanic.full$Age, na.rm=TRUE)
titanic.full[is.na(titanic.full$Age), "Age"]<- age.median

#Cleaning Fare
table(is.na(titanic.full$Fare))
fare.median<- median(titanic.full$Fare, na.rm=TRUE)
titanic.full[is.na(titanic.full$Fare), "Fare"]<- fare.median

#Categorical casting
titanic.full$Pclass<- as.factor(titanic.full$Pclass)
titanic.full$Sex<- as.factor(titanic.full$Sex)
titanic.full$Embarked<- as.factor(titanic.full$Embarked)

#Splitting train and test
titanic.train<- titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test<- titanic.full[titanic.full$IsTrainSet==FALSE,]


titanic.train$Survived<- as.factor(titanic.train$Survived)

survived.equation<- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula<- as.formula(survived.equation)
install.packages("randomForest")
library(randomForest)

titanic.model<- randomForest(formula= survived.formula, data= titanic.train, ntree=500, mtry=3, nodesize= 0.01* nrow(titanic.test))

features.equation<- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived<- predict(titanic.model, newdata= titanic.test)

PassengerId <- titanic.test$PassengerId
output.df<- as.data.frame(PassengerId)
output.df$Survived<- Survived

write.csv(output.df, file="kaggle_ans.csv", row.names = FALSE)
