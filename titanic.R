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
train$Name <- as.character(train$Name)

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

#there is some unbalance from embarked
tr %>%
  group_by(Embarked) %>%
  summarise(sum(as.numeric(Survived))/n(),sum(as.numeric(Survived)), n())

set.seed(1)
rforest <- train(x = tr[2:12],
                 y = tr$Survived,
                 method = "rf")
#from https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md
missmap(tr, main="Titanic Training Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)

barplot(table(tr$Survived),
        names.arg = c("Perished", "Survived"),
        main="Survived (passenger fate)", col="black")
barplot(table(tr$Pclass), 
        names.arg = c("first", "second", "third"),
        main="Pclass (passenger traveling class)", col="firebrick")
barplot(table(tr$Sex), main="Sex (gender)", col="darkviolet")
hist(tr$Age, main="Age", xlab = NULL, col="brown")
barplot(table(tr$SibSp), main="SibSp (siblings + spouse aboard)", 
        col="darkblue")
barplot(table(tr$Parch), main="Parch (parents + kids aboard)", 
        col="gray50")
hist(tr$Fare, main="Fare (fee paid for ticket[s])", xlab = NULL, 
     col="darkgreen")
barplot(table(tr$Embarked), 
        names.arg = c("Cherbourg", "Queenstown", "Southampton"),
        main="Embarked (port of embarkation)", col="sienna")

mosaicplot(tr$Pclass ~ tr$Survived, 
           main="Passenger Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")
mosaicplot(tr$Sex ~ tr$Survived, 
           main="Passenger Fate by Gender", shade=FALSE, color=TRUE, 
           xlab="Sex", ylab="Survived")
mosaicplot(tr$Embarked ~ tr$Survived, 
           main="Passenger Fate by Port of Embarkation",
           shade=FALSE, color=TRUE, xlab="Embarked", ylab="Survived")


getTitle <- function(data) {
  title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
  title.comma.end <- title.dot.start + attr(title.dot.start, "match.length")-1
  data$Title <- substr(data$Name, title.dot.start+2, title.comma.end-1)
  return (data$Title)
}   
tr$Title <- getTitle(tr)
unique(tr$Title)
tr$Title

titles.na.train <- c("Dr", "Master", "Mrs", "Miss", "Mr")
imputeMedian <- function(impute.var, filter.var, var.levels) {
  for (v in var.levels) {
    impute.var[ which( filter.var == v)] <- impute(impute.var[ 
      which( filter.var == v)])
  }
  return (impute.var)
}

tr$Embarked[which(is.na(tr$Embarked))] <- 'S'
tr$Fare[ which( tr$Fare == 0 )] <- NA
tr$Fare <- imputeMedian(tr$Fare, tr$Pclass, 
                              as.numeric(levels(tr$Pclass)))
tr$Title <- factor(tr$Title,
                         c("Capt","Col","Major","Sir","Lady","Rev",
                           "Dr","Don","Jonkheer","the Countess","Mrs",
                           "Ms","Mr","Mme","Mlle","Miss","Master"))
boxplot(tr$Age ~ tr$Title, 
        main="Passenger Age by Title", xlab="Title", ylab="Age")

tr$Family <- tr$SibSp + tr$Parch
tr$Class <- tr$
set.seed(23)
training.rows <- createDataPartition(tr$Survived, 
                                     p = 0.8, list = FALSE)
train.batch <- tr[training.rows, ]
test.batch <- tr[-training.rows, ]

Titanic.logit.1 <- glm(Survived ~ Sex + Pclass + Age + Family + Embarked + Fare, 
                       data = train.batch, family=binomial("logit"))

cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)
set.seed(35)
glm.tune.1 <- train(Survived ~ Sex + Pclass + Age + Family + Embarked,
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)

isEven <- function(x) x %in% c("0","2","4","6","8") 
## test a character as an ODD single digit
isOdd <- function(x) x %in% c("1","3","5","7","9") 

## function to add features to training or test data frames
featureEngrg <- function(data) {
  ## Using Fate ILO Survived because term is shorter and just sounds good
  data$Fate <- data$Survived
  ## Revaluing Fate factor to ease assessment of confusion matrices later
  data$Fate <- revalue(data$Fate, c("1" = "Survived", "0" = "Perished"))
  ## Boat.dibs attempts to capture the "women and children first"
  ## policy in one feature.  Assuming all females plus males under 15
  ## got "dibs' on access to a lifeboat
  data$Boat.dibs <- "No"
  data$Boat.dibs[which(data$Sex == "female" | data$Age < 15)] <- "Yes"
  data$Boat.dibs <- as.factor(data$Boat.dibs)
  ## Family consolidates siblings and spouses (SibSp) plus
  ## parents and children (Parch) into one feature
  data$Family <- data$SibSp + data$Parch
  ## Fare.pp attempts to adjust group purchases by size of family
  data$Fare.pp <- data$Fare/(data$Family + 1)
  ## Giving the traveling class feature a new look
  data$Class <- data$Pclass
  data$Class <- revalue(data$Class, 
                        c("1"="First", "2"="Second", "3"="Third"))
  ## First character in Cabin number represents the Deck 
  data$Deck <- substring(data$Cabin, 1, 1)
  data$Deck[ which( is.na(data$Deck ))] <- "UNK"
  data$Deck <- as.factor(data$Deck)
  ## Odd-numbered cabins were reportedly on the port side of the ship
  ## Even-numbered cabins assigned Side="starboard"
  data$cabin.last.digit <- str_sub(data$Cabin, -1)
  data$Side <- "UNK"
  data$Side[which(isEven(data$cabin.last.digit))] <- "port"
  data$Side[which(isOdd(data$cabin.last.digit))] <- "starboard"
  data$Side <- as.factor(data$Side)
  data$cabin.last.digit <- NULL
  return (data)
}
df.train <- featureEngrg(train)
train.keeps <- c("Fate", "Sex", "Boat.dibs", "Age", "Title", 
                 "Class", "Deck", "Side", "Fare", "Fare.pp", 
                 "Embarked", "Family")
df.train.munged <- df.train[train.keeps]