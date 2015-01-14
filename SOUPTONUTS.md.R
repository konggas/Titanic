#### Titanic Survival Prediction

readData <- function(path.name, file.name, column.types, missing.types) {
  read.csv( paste(path.name, file.name, sep="") , 
            colClasses=column.types,
            na.strings=missing.types )
}
Titanic.path <- "/Users/konggas/titanic/Titanic/"
train.data.file <- "train.csv"
test.data.file <- "test.csv"
missing.types <- c("NA", "")
train.column.types <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)
test.column.types <- train.column.types[-2]     # # no Survived column in test.csv
train.raw <- readData(Titanic.path, train.data.file, 
                      train.column.types, missing.types)
df.train <- train.raw

test.raw <- readData(Titanic.path, test.data.file, 
                     test.column.types, missing.types)
df.infer <- test.raw   
require(Amelia)
missmap(df.train, main="Titanic Training Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)
barplot(table(df.train$Survived),
        names.arg = c("Perished", "Survived"),
        main="Survived (passenger fate)", col="black")
barplot(table(df.train$Pclass), 
        names.arg = c("first", "second", "third"),
        main="Pclass (passenger traveling class)", col="firebrick")
barplot(table(df.train$Sex), main="Sex (gender)", col="darkviolet")
hist(df.train$Age, main="Age", xlab = NULL, col="brown")
barplot(table(df.train$SibSp), main="SibSp (siblings + spouse aboard)", 
        col="darkblue")
barplot(table(df.train$Parch), main="Parch (parents + kids aboard)", 
        col="gray50")
hist(df.train$Fare, main="Fare (fee paid for ticket[s])", xlab = NULL, 
     col="darkgreen")
barplot(table(df.train$Embarked), 
        names.arg = c("Cherbourg", "Queenstown", "Southampton"),
        main="Embarked (port of embarkation)", col="sienna")
mosaicplot(df.train$Pclass ~ df.train$Survived, 
           main="Passenger Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")
mosaicplot(df.train$Sex ~ df.train$Survived, 
           main="Passenger Fate by Gender", shade=FALSE, color=TRUE, 
           xlab="Sex", ylab="Survived")
boxplot(df.train$Age ~ df.train$Survived, 
        main="Passenger Fate by Age",
        xlab="Survived", ylab="Age")
mosaicplot(df.train$Embarked ~ df.train$Survived, 
           main="Passenger Fate by Port of Embarkation",
           shade=FALSE, color=TRUE, xlab="Embarked", ylab="Survived")
require(corrgram)
corrgram.data <- df.train
corrgram.data$Survived <- as.numeric(corrgram.data$Survived)
corrgram.data$Pclass <- as.numeric(corrgram.data$Pclass)
corrgram.data$Embarked <- revalue(corrgram.data$Embarked, 
                                  c("C" = 1, "Q" = 2, "S" = 3))
corrgram.vars <- c("Survived", "Pclass", "Sex", "Age", 
                   "SibSp", "Parch", "Fare", "Embarked")
corrgram(corrgram.data[,corrgram.vars], order=FALSE, 
         lower.panel=panel.ellipse, upper.panel=panel.pie, 
         text.panel=panel.txt, main="Titanic Training Data")
summary(df.train$Age)
names(df.train)
head(df.train$Name, n=10L)
getTitle <- function(data) {
  title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
  title.comma.end <- title.dot.start + attr(title.dot.start, "match.length")-1
  print(title.comma.end)
  data$Title <- substr(data$Name, title.dot.start+2, title.comma.end-1)
  return (data$Title)
}     
df.train$Title <- getTitle(df.train)
unique(df.train$Title)
options(digits=2)
require(Hmisc)
bystats(df.train$Age, df.train$Title, 
        fun=function(x)c(Mean=mean(x),Median=median(x)))
titles.na.train <- c("Dr", "Master", "Mrs", "Miss", "Mr")
imputeMedian <- function(impute.var, filter.var, var.levels) {
  for (v in var.levels) {
    impute.var[ which( filter.var == v)] <- impute(impute.var[ 
      which( filter.var == v)])
  }
  return (impute.var)
}
df.train$Age[which(df.train$Title=="Dr")]
df.train$Age <- imputeMedian(df.train$Age, df.train$Title, 
                             titles.na.train)
df.train$Age[which(df.train$Title=="Dr")]
summary(df.train$Age)
summary(df.train$Embarked)
df.train$Embarked[which(is.na(df.train$Embarked))] <- 'S'
summary(df.train$Fare)
subset(df.train, Fare < 7)[order(subset(df.train, Fare < 7)$Fare, 
                          subset(df.train, Fare < 7)$Pclass), 
                          c("Age", "Title", "Pclass", "Fare")]
df.train$Fare[ which( df.train$Fare == 0 )] <- NA
df.train$Fare <- imputeMedian(df.train$Fare, df.train$Pclass, 
                              as.numeric(levels(df.train$Pclass)))

boxplot(df.train$Age ~ df.train$Title, 
        main="Passenger Age by Title", xlab="Title", ylab="Age")

changeTitles <- function(data, old.titles, new.title) {
  for (honorific in old.titles) {
    data$Title[ which( data$Title == honorific)] <- new.title
  }
  return (data$Title)
}
## Title consolidation
df.train$Title <- changeTitles(df.train, 
                               c("Capt", "Col", "Don", "Dr","Jonkheer", "Lady", "Major", "Rev", "Sir"),
                               "Noble")
df.train$Title <- changeTitles(df.train, c("the Countess", "Ms"), 
                               "Mrs")
df.train$Title <- changeTitles(df.train, c("Mlle", "Mme"), "Miss")
df.train$Title <- as.factor(df.train$Title)
df.train$Title <- factor(df.train$Title,
                         c("Capt","Col","Major","Sir","Lady","Rev",
                           "Dr","Don","Jonkheer","the Countess","Mrs",
                           "Ms","Mr","Mme","Mlle","Miss","Master"))
require(plyr)     # for the revalue function 
require(stringr)  # for the str_sub function

## test a character as an EVEN single digit
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

## add remaining features to training data frame
df.train <- featureEngrg(df.train)
train.keeps <- c("Fate", "Sex", "Boat.dibs", "Age", "Title", 
                 "Class", "Deck", "Side", "Fare", "Fare.pp", 
                 "Embarked", "Family")
df.train.munged <- df.train[train.keeps]





## Fitting a Model
set.seed(23)
training.rows <- createDataPartition(df.train.munged$Fate, 
                                     p = 0.8, list = FALSE)
train.batch <- df.train.munged[training.rows, ]
test.batch <- df.train.munged[-training.rows, ]
Titanic.logit.1 <- glm(Fate ~ Sex + Class + Age + Family + Embarked + Fare, 
                       data = train.batch, family=binomial("logit")
Titanic.logit.1

1 - pchisq(332.2, df=8)
anova(Titanic.logit.1, test="Chisq")
Titanic.logit.2 <- glm(Fate ~ Sex + Class + Age + Family + Embarked + Fare.pp,                        
                         data = train.batch, family=binomial("logit"))
anova(Titanic.logit.2, test="Chisq")

glm(Fate ~ Sex + Class + Age + Family + Embarked, 
      data = train.batch, family=binomial("logit"))
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)


set.seed(35)
glm.tune.1 <- train(Fate ~ Sex + Class + Age + Family + Embarked,
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)
summary(glm.tune.1)



set.seed(35)
glm.tune.2 <- train(Fate ~ Sex + Class + Age + Family + I(Embarked=="S"),
                      data = train.batch, method = "glm",
                      metric = "ROC", trControl = cv.ctrl)
summary(glm.tune.2)



set.seed(35)
glm.tune.3 <- train(Fate ~ Sex + Class + Title + Age 
                      + Family + I(Embarked=="S"), 
                      data = train.batch, method = "glm",
                      metric = "ROC", trControl = cv.ctrl)
summary(glm.tune.3)



set.seed(35)
glm.tune.4 <- train(Fate ~ Class + I(Title=="Mr") + I(Title=="Noble") 
                      + Age + Family + I(Embarked=="S"), 
                      data = train.batch, method = "glm",
                      metric = "ROC", trControl = cv.ctrl)
summary(glm.tune.4)



set.seed(35)
glm.tune.5 <- train(Fate ~ Class + I(Title=="Mr") + I(Title=="Noble") 
                      + Age + Family + I(Embarked=="S") 
                      + I(Title=="Mr"&Class=="Third"), 
                      data = train.batch, 
                      method = "glm", metric = "ROC", 
                      trControl = cv.ctrl)
summary(glm.tune.5)



#### Other Models
#Logistic regression is certainly not the only binary classification model available.  There are plenty more –- perhaps too many for some data scientists to digest.  For purpose of illustration, I'll simply take the logistic regression model formula from **glm.tune.1** and pass it through  ``` train()``` for each of three other model types, with one new twist:  tuning variables specific to each model.   

#First up is [boosting](http://en.wikipedia.org/wiki/AdaBoost).  I can instruct ``` train``` to fit a *stochastic boosting* model for the binary response **Fate** using the ``` ada```package and a range of values for each of three tuning parameters.  Concretely, when fitting a model using ``` train``` with ``` method=”ada”```, one has three levers to tweak: ``` iter``` (number of boosting iterations, default=50), ``` maxdepth``` (depth of trees), and ``` nu``` (shrinkage parameter, default=1).  Create a data frame with these three variables as column names and one row per tuning variable combination, and you're good to go.  Here is just one example of a tuning grid for ``` ada```:
#```sh
## note the dot preceding each variable
ada.grid <- expand.grid(.iter = c(50, 100),
                        .maxdepth = c(4, 8),
                        .nu = c(0.1, 1))
#```
#Specify ``` method=”ada”``` and ``` tuneGrid=ada.grid``` in ``` train```, and away we go...
#```sh
set.seed(35)
ada.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked, 
                  data = train.batch,
                  method = "ada",
                  metric = "ROC",
                  tuneGrid = ada.grid,
                  trControl = cv.ctrl)
#```
#The model output shows that, given the **train.batch** data and 8 combinations of tuning variables tested,  the optimal model had an ROC of 0.871.  The tuning parameter values used to build that model were ``` iter = 100```, ``` maxdepth = 4```, and ``` nu = 0.1```. 
#```sh
ada.tune

plot(ada.tune)     ## ada accuracy profile
```
![alt text](http://drive.google.com/uc?export=view&id=0B-yx9UUIpB6uLWhoNGJXVlJzLUU)

Time to give the popular **Random Forest** (RF) model a shot at the Titanic challenge.  The number of randomly pre-selected predictor variables for each node, designated ``` mtry```, is the sole parameter available for tuning an RF with ``` train```.  Since the number of features is so small, there really isn't much scope for tuning ``` mtry``` in this case.  Nevertheless, I'll demonstrate here how it can be done. Let's have  ``` mtry=2``` and ```mtry=3``` duke it out over the Titanic data.
```sh
rf.grid <- data.frame(.mtry = c(2, 3))
set.seed(35)
rf.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked, 
                 data = train.batch,
                 method = "rf",
                 metric = "ROC",
                 tuneGrid = rf.grid,
                 trControl = cv.ctrl)
```
[Strobl et al](http://www.ncbi.nlm.nih.gov/pubmed/19968396) suggested setting ``` mtry``` at the square root of the number of variables.  In this case, that would be ``` mtry = 2```, which did produce the better RF model.
```sh
> rf.tune
714 samples
 11 predictors
  2 classes: 'Perished', 'Survived' 

No pre-processing
Resampling: Cross-Validation (10 fold, repeated 3 times) 

Summary of sample sizes: 643, 643, 643, 642, 643, 643, ... 

Resampling results across tuning parameters:

  mtry  ROC    Sens   Spec   ROC SD  Sens SD  Spec SD
  2     0.866  0.952  0.633  0.052   0.0288   0.0945 
  3     0.861  0.934  0.642  0.0514  0.0345   0.0916 

ROC was used to select the optimal model using  the largest value.
The final value used for the model was mtry = 2. 
```
And finally, we'll fit a **support vector machine (SVM)** model to the Titanic data.  There are two functions which can be tuned for SVM using ``` train```.  The default value for one of them -– ``` sigest``` –- produces good results on most occasions.  The default grid of cost parameter C  is 0.25, 0.5,  and 1.  If we set ``` train``` argument ``` tuneLength = 9```, the grid expands to c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64).  As SVM is considered sensitive to the scale and magnitude of the presented features, I'll use the ``` preProcess``` argument to instruct ``` train``` to make arrangements for [normalizing](http://en.wikipedia.org/wiki/Feature_scaling) the data within resampling loops.
```sh
set.seed(35)
svm.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked, 
                  data = train.batch,
                  method = "svmRadial",
                  tuneLength = 9,
                  preProcess = c("center", "scale"),
                  metric = "ROC",
                  trControl = cv.ctrl)
```
You may have noticed that the same random number seed was set prior to fitting each model. This ensures that the same resampling sets are used for each model, enabling an "apple-to-apples" comparison of the resampling profiles between models during model evaluation.
```sh
> svm.tune
714 samples
 11 predictors
  2 classes: 'Perished', 'Survived' 

Pre-processing: centered, scaled 
Resampling: Cross-Validation (10 fold, repeated 3 times) 

Summary of sample sizes: 643, 643, 643, 642, 643, 643, ... 

Resampling results across tuning parameters:

  C     ROC    Sens   Spec   ROC SD  Sens SD  Spec SD
  0.25  0.832  0.951  0.628  0.0609  0.0274   0.0948 
  0.5   0.833  0.947  0.629  0.0627  0.0282   0.0966 
  1     0.833  0.944  0.639  0.0589  0.032    0.0904 
  2     0.835  0.936  0.645  0.0623  0.0398   0.0892 
  4     0.826  0.933  0.644  0.0615  0.0426   0.0935 
  8     0.824  0.932  0.64   0.0568  0.0418   0.0845 
  16    0.82   0.923  0.634  0.0553  0.0441   0.0867 
  32    0.803  0.915  0.633  0.0617  0.0386   0.0876 
  64    0.788  0.906  0.626  0.056   0.0367   0.0855 

Tuning parameter 'sigma' was held constant at a value of 0.2204311
ROC was used to select the optimal model using  the largest value.
The final values used for the model were C = 2 and sigma = 0.22. 
```
Although the model output above does display ROC by cost parameter value, the following graph makes it abundantly clear that the ROC starts dropping at C=4.
![alt text](http://drive.google.com/uc?export=view&id=0B-yx9UUIpB6uejJwNmlXaEg4N2c)


### Model Evaluation

With all four models in hand, I can begin to evaluate their performance by whipping together some cross-tabulations of the observed and predicted **Fate** for the passengers in the **test.batch** data.  ``` caret``` makes this easy with the ``` confusionMatrix``` function.
```sh
## Logistic regression model
> glm.pred <- predict(glm.tune.5, test.batch)
> confusionMatrix(glm.pred, test.batch$Fate)
Confusion Matrix and Statistics

          Reference
Prediction Perished Survived
  Perished       97       19
  Survived       12       49
                                          
               Accuracy : 0.8249          
                 95% CI : (0.7607, 0.8778)
    No Information Rate : 0.6158          
    P-Value [Acc > NIR] : 1.304e-09       
                                          
                  Kappa : 0.6225          
 Mcnemar's Test P-Value : 0.2812          
                                          
            Sensitivity : 0.8899          
            Specificity : 0.7206          
         Pos Pred Value : 0.8362          
         Neg Pred Value : 0.8033          
             Prevalence : 0.6158          
         Detection Rate : 0.5480          
   Detection Prevalence : 0.6554
```
```sh
## Boosted model
> ada.pred <- predict(ada.tune, test.batch)
> confusionMatrix(ada.pred, test.batch$Fate)
Confusion Matrix and Statistics

          Reference
Prediction Perished Survived
  Perished      100       23
  Survived        9       45
                                          
               Accuracy : 0.8192          
                 95% CI : (0.7545, 0.8729)
    No Information Rate : 0.6158          
    P-Value [Acc > NIR] : 3.784e-09       
                                          
                  Kappa : 0.6025          
 Mcnemar's Test P-Value : 0.02156         
                                          
            Sensitivity : 0.9174          
            Specificity : 0.6618          
         Pos Pred Value : 0.8130          
         Neg Pred Value : 0.8333          
             Prevalence : 0.6158          
         Detection Rate : 0.5650          
   Detection Prevalence : 0.6949
```
```sh
## Random Forest model
> rf.pred <- predict(rf.tune, test.batch)
> confusionMatrix(rf.pred, test.batch$Fate)
Confusion Matrix and Statistics

          Reference
Prediction Perished Survived
  Perished      103       27
  Survived        6       41
                                          
               Accuracy : 0.8136          
                 95% CI : (0.7483, 0.8681)
    No Information Rate : 0.6158          
    P-Value [Acc > NIR] : 1.058e-08       
                                          
                  Kappa : 0.5817          
 Mcnemar's Test P-Value : 0.0004985       
                                          
            Sensitivity : 0.9450          
            Specificity : 0.6029          
         Pos Pred Value : 0.7923          
         Neg Pred Value : 0.8723          
             Prevalence : 0.6158          
         Detection Rate : 0.5819          
   Detection Prevalence : 0.7345 
```
```sh
## SVM model 
> svm.pred <- predict(svm.tune, test.batch)
> confusionMatrix(svm.pred, test.batch$Fate)
Confusion Matrix and Statistics

          Reference
Prediction Perished Survived
  Perished      101       27
  Survived        8       41
                                          
               Accuracy : 0.8023          
                 95% CI : (0.7359, 0.8582)
    No Information Rate : 0.6158          
    P-Value [Acc > NIR] : 7.432e-08       
                                          
                  Kappa : 0.5589          
 Mcnemar's Test P-Value : 0.002346        
                                          
            Sensitivity : 0.9266          
            Specificity : 0.6029          
         Pos Pred Value : 0.7891          
         Neg Pred Value : 0.8367          
             Prevalence : 0.6158          
         Detection Rate : 0.5706          
   Detection Prevalence : 0.7232
```
(Perhaps now you've come to appreciate why I revalued the **Fate** feature earlier!)  While there are no convincing conclusions to be drawn from the confusion matrices embedded within the outputs above, the logistic regression model we put together earlier appears to do the best job of selecting the survivors among the passengers in the test.batch.  The Random Forest model, on the other hand, seems to have a slight edge on predicting those who perished.  

We can also calculate, using each of the four fitted models, the predicted probabilities for the **test.batch**, and use those probabilities to plot the ROC curves.
```sh
## Logistic regression model (BLACK curve)
glm.probs <- predict(glm.tune.5, test.batch, type = "prob")
glm.ROC <- roc(response = test.batch$Fate,
                predictor = glm.probs$Survived,
                levels = levels(test.batch$Fate))
plot(glm.ROC, type="S")   
## Area under the curve: 0.8609 
```
```sh
## Boosted model (GREEN curve)
ada.probs <- predict(ada.tune, test.batch, type = "prob")
ada.ROC <- roc(response = test.batch$Fate,
            predictor = ada.probs$Survived,
            levels = levels(test.batch$Fate))
plot(ada.ROC, add=TRUE, col="green")    
## Area under the curve: 0.8759
```
```sh
## Random Forest model (RED curve)
rf.probs <- predict(rf.tune, test.batch, type = "prob")
rf.ROC <- roc(response = test.batch$Fate,
           predictor = rf.probs$Survived,
           levels = levels(test.batch$Fate))
plot(rf.ROC, add=TRUE, col="red") 
## Area under the curve: 0.8713
```
```sh
## SVM model (BLUE curve)
svm.probs <- predict(svm.tune, test.batch, type = "prob")
svm.ROC <- roc(response = test.batch$Fate,
            predictor = svm.probs$Survived,
            levels = levels(test.batch$Fate))
plot(svm.ROC, add=TRUE, col="blue")
## Area under the curve: 0.8077
```
![alt text](http://drive.google.com/uc?export=view&id=0B-yx9UUIpB6uTlhEZFpOOU5PVTA)

The following R script uses ``` caret``` function ``` resamples``` to collect the resampling results, then calls the  ``` dotplot``` function to create a visualization of the resampling distributions.  I'm typically not one for leaning on a single metric for important decisions, but if you have been looking for that one graph which sums up the performance of the four models, this is it.  
```sh
cv.values <- resamples(list(Logit = glm.tune.5, Ada = ada.tune, 
                            RF = rf.tune, SVM = svm.tune))
dotplot(cv.values, metric = "ROC")
```
![alt text](http://drive.google.com/uc?export=view&id=0B-yx9UUIpB6uaEJzZFYzSmpEQWs)

The next graph (my last, scout's honor) compares the four models on the basis of ROC, sensitivity, and specificity.  Here, sensitivity (“Sens” on the graph) is the probability that a model will predict a Titanic passenger's death, given that the passenger actually did perish.  Think of sensitivity in this case as the true perished rate.  Specificity (“Spec”), on the other hand, is the probability that a model will predict survival, given that the passenger actually did survive.  Simply put, all four models were better at predicting passenger fatalities than survivals, and none of them are significantly better or worse than the other three. Of the four, if I *had* to pick one, I'd probably put my money on the logistic regression model.
![alt text](http://drive.google.com/uc?export=view&id=0B-yx9UUIpB6uNVVsQXh2RVA2LUk)

Let me reiterate the point I made in the disclaimer, *way* up at the top of this tl;dr page:  This journey, paved with text and graphs, was never intended to reveal a path to discovery of the best model for predicting the fate of the passengers referenced in the Titanic data set.  I sought only to demonstrate use of a subset of the tools – methods and software (R in this case) –  a data scientist can employ in pursuit of a binary classification model. 

### Cast Your Votes

Given everything we've been through here, it would be a shame if we didn't submit at least one of the four models to the [Titanic competition at Kaggle](http://www.kaggle.com/c/titanic-gettingStarted).  Here is a script which munges the data Kaggle provided in their test.csv file, uses that data and the logistic regression model **glm.tune.5** to predict the survival (or not) of passengers listed in the test file, links the predictions to the PassengerId in a data frame, and writes those results to a submission-ready csv file. 
```sh
# get titles
df.infer$Title <- getTitle(df.infer)

# impute missing Age values
df.infer$Title <- changeTitles(df.infer, c("Dona", "Ms"), "Mrs")
titles.na.test <- c("Master", "Mrs", "Miss", "Mr")
df.infer$Age <- imputeMedian(df.infer$Age, df.infer$Title, titles.na.test)

# consolidate titles
df.infer$Title <- changeTitles(df.infer, c("Col", "Dr", "Rev"), "Noble")
df.infer$Title <- changeTitles(df.infer, c("Mlle", "Mme"), "Miss")
df.infer$Title <- as.factor(df.infer$Title)

# impute missing fares
df.infer$Fare[ which( df.infer$Fare == 0)] <- NA
df.infer$Fare <- imputeMedian(df.infer$Fare, df.infer$Pclass, 
                                as.numeric(levels(df.infer$Pclass)))
# add the other features
df.infer <- featureEngrg(df.infer)

# data prepped for casting predictions
test.keeps <- train.keeps[-1]
pred.these <- df.infer[test.keeps]

# use the logistic regression model to generate predictions
Survived <- predict(glm.tune.5, newdata = pred.these)

# reformat predictions to 0 or 1 and link to PassengerId in a data frame
Survived <- revalue(Survived, c("Survived" = 1, "Perished" = 0))
predictions <- as.data.frame(Survived)
predictions$PassengerId <- df.infer$PassengerId

# write predictions to csv file for submission to Kaggle
write.csv(predictions[,c("PassengerId", "Survived")], 
          file="Titanic_predictions.csv", row.names=FALSE, quote=FALSE)
```

If you must know, the logistic regression model scored 0.77990 on Kaggle – roughly middle of the pack on [the leaderboard](http://www.kaggle.com/c/titanic-gettingStarted/leaderboard) (as of late January 2014).  I have submitted better scoring models; my best thus far, at 0.79426, trails just 17 percent of the 1150+ participants on the leaderboard.  

While I'm certain that I could squeeze more out of a model like Random Forest to improve my Kaggle ranking, I see better uses of my time.  The correlation between public scores and final scores at Kaggle competitions is historically poor (see [this post](http://www.rouli.net/2013/02/five-lessons-from-kaggles-event.html)).  Besides, I'd rather devote more time helping people with *today's* challenges.
