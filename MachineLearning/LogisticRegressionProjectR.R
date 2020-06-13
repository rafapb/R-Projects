# Data from: http://archive.ics.uci.edu/ml/datasets/Adult

# The main purpose of this script is to do some exploratory data analysis and
# build a simple logistic regression model to predict if people in the dataset 
# are either making <= 50k or > 50k per year. 

library(ggplot2)
library(dplyr)

adult <- read.csv("CSV files for ML Projects/adult_sal.csv")
head(adult)

adult <- select(adult,-X)
head(adult)
str(adult)
summary(adult)

table(adult$type_employer)

# DATA CLEANING - GROUP UNEMPLOYED

unemp <- function(job){
  job <- as.character(job)
  if (job=="Never-worked" | job=="Without-pay"){
    return("Unemployed")
  }else{
    return(job)
  }
}

# APPLY

adult$type_employer <- sapply(adult$type_employer, unemp)

table(adult$type_employer)

# DATA CLENAING - GROUP SELF EMPLOYED

group_emp <- function(job){
  if (job=='Local-gov' | job=='State-gov'){
    return('SL-gov')
  }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('self-emp')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer, group_emp)

table(adult$type_employer)

# DATA CLEANING - MARTIAL STATUS

table(adult$marital)

group_marital <- function(mar){
  mar <- as.character(mar)
  # Not-Married
  if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')
    # Never-Married   
  }else if(mar=='Never-married'){
    return(mar)
    #Married
  }else{
    return('Married')
  }
}

adult$marital <- sapply(adult$marital, group_marital)

table(adult$marital)

# DATA CLEANING - COUNTRY

table(adult$country)

# VECTORS OF REGIONS

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

# GROUP COUNTRY FUNCTION

group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

adult$country <- sapply(adult$country,group_country)

table(adult$country)

# REFACTOR AFFECTED COLUMNS

adult[adult=="?"] <- NA

str(adult)

adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult$occupation <- sapply(adult$occupation,factor)
adult$relationship <- sapply(adult$relationship,factor)
adult$race <- sapply(adult$race,factor)
adult$sex <- sapply(adult$sex,factor)
adult$income <- sapply(adult$income,factor)
adult$education <- sapply(adult$education,factor)

table(adult$type_employer)

# MISSING DATA

library(Amelia)

missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

# DROP MISSING DATA

adult <- na.omit(adult)

missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

# EDA

ggplot(adult, aes(age)) + geom_histogram(aes(fill=income), color="black", binwidth=1) + theme_bw()

ggplot(adult,aes(hr_per_week)) + geom_histogram() + theme_bw()

# RENAME COUNTRY TO REGION

names(adult)[names(adult)=="country"] <- "region"


ggplot(adult,aes(region)) + geom_bar(aes(fill=income),color='black')+theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# LOGISTIC REGRESSION MODEL

# TRAIN TEST SPLIT

library(caTools)

set.seed(101)

sample <- sample.split(adult$income, SplitRatio = 0.7)

train <- subset(adult, sample == TRUE)
test <- subset(adult, sample == FALSE)

model <- glm(income ~ . , family = binomial(link = 'logit'), data=train)

summary(model)

# NEW MODEL USING A STEP FUNCTION

new.step.model <- step(model)

# PREDICTED INCOME

test$predicted.income <- predict(model, newdata = test, type = 'response')

# CONFUSION MATRIX

table(test$income, test$predicted.income > 0.5)

# ACCURACY

accuracy <- (6372+1423)/(6372+1423+548+872)

# RECALL

recall <- (6372)/(6372+548)

# PRECISION

precision <- (6372)/(6372+872)

# END