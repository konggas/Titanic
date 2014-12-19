gender_class<-read.csv("./titanic/genderclassmodel.csv")
gender_model<-read.csv("./titanic/gendermodel.csv")
test<-read.csv("./titanic/test.csv")
train<-read.csv("./titanic/train.csv")

#te<-test[complete.cases(test),]
x<-na.omit(test)
names(test)

test$gen <- factor(test$Sex)
test$gen
sum(is.na(test['Age'])) #5th column, 86 missing
which(is.na(test['Fare']) #9th column, 1 missing
#Cabin column has a lot of missing






te.gender <-factor(te.sex)
