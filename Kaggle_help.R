readData <- function(path.name, file.name, column.types, missing.types) {
  urlfile<-paste(path.name,file.name,sep='')
  print (urlfile)
  #download.file(urlfile, 
  #              destfile = "/tmp/test.csv", method = "curl")
  read.csv( "~/titanic/Titanic/test.csv", colClasses=column.types,
            na.strings=missing.types)
}


Titanic.path <- "https://raw.githubusercontent.com/konggas/Titanic/master/"
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

