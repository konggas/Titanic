gender_class<-read.csv("./titanic/genderclassmodel.csv")
gender_model<-read.csv("./titanic/gendermodel.csv")
test<-read.csv("~/titanic/Titanic/test.csv")
train<-read.csv("~/titanic/Titanic/train.csv")

#te<-test[complete.cases(test),]
#x<-na.omit(train)
names(train)


train$Sex <- as.factor(train$Sex)
train$Pclass <- as.factor(train$Pclass)
train$Cabin <- as.factor(train$Cabin)
train$Survived <- as.factor(train$Survived)

sum(is.na(test['Age'])) #5th column, 86 missing
which(is.na(test['Fare']) #9th column, 1 missing
#Cabin column has a lot of missing

#qplot(Age,data=train,fill=Sex)
qplot(Cabin,Pclass,data=train,color=Survived)
g<-ggplot(train,aes(Cabin,Pclass))
  g+geom_point(aes(color=Survived),size=4,alpha=1/2)
qplot(Embarked,data=train,fill=Survived)
qplot(Embarked,Fare,data=train,color=Survived)
qplot(Fare,data=train,fill=Survived)
qplot(Fare,data=train,facets=.~Sex,fill=Survived,binwidth=10)#some trend
qplot(Pclass,Age,data=train,color=Survived,facets=.~Sex)

tr <- tbl_df(train)

tr %>%
  group_by(Pclass, Sex) %>%
  summarise(srate = (sum(as.numeric(Survived)-1))/n())

tr %>%
  group_by(Cabin, Sex) %>%
  summarise(srate = (sum(as.numeric(Survived)-1))/n())

tr %>%
  group_by(Cabin, Sex) %>%
  tally()
  
set.seed(1)
rforest <- train(x = tr[2:12],
                 y = tr$Survived,
                 method = "rf")
