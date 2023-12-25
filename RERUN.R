#IMPORT train.csv to train
#IMPORT test.csv to TEST
#WE ARE TEAM JARVIS. WELCOME TO THE MATRIX
library(tidyverse)

train=read.csv("D:/Mini Hackathon/Prelims/train.csv")
train.backup=read.csv("D:/Mini Hackathon/Prelims/train.csv")
TEST=read.csv("D:/Mini Hackathon/Prelims//test.csv")
TEST.backup=read.csv("D:/Mini Hackathon/Prelims//test.csv")
head(train)
head(TEST)


df=train
df.TEST=TEST

summary(df)
summary(df.TEST)

#There are no missing values
#THere are 4200 outlets

#Checking Variable Sturctures
fs=factor(train$freezer_status)
pie(table(fs))
levels(fs)

#Checking WeekDate
date_column=train$week_start_date

#Function to check if a date is valid
is_valid_date=function(date_str){
  tryCatch({
    as.Date(date_str)
    return(TRUE)
  },error=function(e){
    return(FALSE)
  })
}

#Check for invalid dates
invalid_dates=date_column[!sapply(date_column, is_valid_date)]

#Display invalid dates
if(length(invalid_dates)>0) {
  cat("Invalid dates found:\n")
  print(invalid_dates)
}else{
  cat("No invalid dates found.\n")
}

#OUtlet Region
head(df$outlet_region)
or=factor(train$outlet_region)
levels(or)
pie(table(or))

#outlet_code
oc=factor(train$outlet_code)
length(oc)
length(levels(oc))

#Number of times each outlet is repeated in the dataset
as.integer(dim(df)[1]/length(levels(oc)))


#Data Cleaning
library(dplyr)

#Changing the format of the expected_rainfall variable
er=df$expected_rainfall
library(stringr)

er=as.numeric(substr(df$expected_rainfall,1,nchar(df$expected_rainfall)-2))
df$expected_rainfall=er
str(df$expected_rainfall)
head(df)

TEST$expected_rainfall=as.numeric(substr(TEST$expected_rainfall,1,nchar(TEST$expected_rainfall)-2))
head(TEST)

#Cleaning freezer_status
fs=df$freezer_status
fs[fs==" no freezers available "]="no freezers available"
fs[fs!="no freezers available"]="freezers available"
summary(factor(fs))
df$freezer_status=factor(fs)
str(df)

levels(factor(TEST$freezer_status))
TEST$freezer_status[TEST$freezer_status==" no freezers available "]="no freezers available"
TEST$freezer_status[TEST$freezer_status!="no freezers available"]="freezers available"
TEST$freezer_status=factor(TEST$freezer_status)

#Getting the dates
dates=as.Date(df$week_start_date,format = "%m/%d/%Y")
df$week_start_date=dates
str(df)

df2 = df %>% arrange(df$week_start_date)

df=df2

TEST$week_start_date=as.Date(TEST$week_start_date,format = "%m/%d/%Y")

#Getting the structue of the cleaned data
str(df)
str(TEST)

#Factorise outler_region and outlet_centre
df$outlet_region=factor(df$outlet_region)
df$outlet_code=factor(df$outlet_code)

TEST$outlet_region=factor(TEST$outlet_region)
TEST$outlet_code=factor(TEST$outlet_code)

str(df)
str(TEST)

x=df$week_start_date

weekno=as.numeric(format(x,"%d"))
daychar=as.character(format(x,"%d"))
monthno=as.numeric(format(x,"%m"))
monthchar=as.character(format(x,"%m"))

#Getting the number of weeks
df$month=monthno
df$Week=weekno
df$Week_No=paste(as.character(df$month),as.character(df$Week),sep = "-")
x=df$Week_No

head(x)
unique(x)
for (i in 1:length(unique(x))){
  x[x==unique(x)[i]]=i
}
df$Week_No=as.numeric(x)
x=factor(df$Week_No)

x=TEST$week_start_date

monthno=as.numeric(format(x,"%m"))

TEST$month=monthno
TEST$Week_No=28

df=subset(df, select = -week_start_date)
summary(df)
str(df)

train=df
train=subset(train, select=-Week)

#Selecting testing and training datasets from original training datasets

number.of.points.for.training=4200*25
main.train=train[1:number.of.points.for.training,]
main.test=train[(number.of.points.for.training+1):dim(train)[1],]

df=main.train

#EDA
library(hexbin)

  
df %>%
  ggplot(aes(x=outlet_region,y=sales_quantity))+
  geom_boxplot()+
  coord_flip()

df %>%
  ggplot(aes(sales_quantity,colours=outlet_region))+
  geom_density()

df %>%
  ggplot(aes(freezer_status,fill=outlet_region))+
  geom_bar(alpha=0.9)

df %>%
  ggplot(aes(sales_quantity,fill=factor(month)))+
  geom_boxplot()+
  coord_flip()

df %>%
  ggplot(aes(sales_quantity,fill=factor(Week_No)))+
  geom_boxplot()+
  coord_flip()
  
agg.sum.week=aggregate(df,sales_quantity~Week_No,sum)

agg.sum.week %>%
  ggplot(aes(x=Week_No,y=sales_quantity))+
  geom_point(size=3)+
  geom_line()

week.region.sales=df[,c("outlet_region","Week_No","sales_quantity")]

week.region.sales=week.region.sales %>%
  arrange(outlet_region,desc(Week_No))
head(week.region.sales)
tail(week.region.sales)

x=week.region.sales %>% 
  group_by(outlet_region,Week_No) %>%
  summarise(Weekly_Regional_Total = sum(sales_quantity))

high_rainfall_threshold= 150 
medium_rainfall_threshold = 50 

# Creating the expected_rainfall_category variable
main.train$expected_rainfall_category= cut(main.train$expected_rainfall,breaks = c(0, medium_rainfall_threshold, high_rainfall_threshold, 200),labels = c("Low", "Medium", "High"),include.lowest = TRUE)
main.test$expected_rainfall_category= cut(main.test$expected_rainfall,breaks = c(0, medium_rainfall_threshold, high_rainfall_threshold, 200),labels = c("Low", "Medium", "High"),include.lowest = TRUE)
TEST$expected_rainfall_category= cut(TEST$expected_rainfall,breaks = c(0, medium_rainfall_threshold, high_rainfall_threshold, 200),labels = c("Low", "Medium", "High"),include.lowest = TRUE)

#Converting to a factor variable
main.train$expected_rainfall_category=factor(main.train$expected_rainfall_category, levels = c("Low", "Medium", "High"))
main.test$expected_rainfall_category=factor(main.test$expected_rainfall_category, levels = c("Low", "Medium", "High"))
TEST$expected_rainfall_category=factor(TEST$expected_rainfall_category, levels = c("Low", "Medium", "High"))

main.train=subset(main.train, select = -expected_rainfall)
main.test=subset(main.test, select = -expected_rainfall)
TEST=subset(TEST, select=-c(week_start_date,expected_rainfall))

#Label Encoding the outlet_code
s=main.train$outlet_code
s=as.numeric(substring(s,first=13))
main.train$outlet_code=s

s=main.test$outlet_code
s=as.numeric(substring(s,first=13))
main.test$outlet_code=s

str(main.test)
str(main.train)

#Making Sales Quantity as Log
main.train.FINAL$sales_quantity=log(main.train.FINAL$sales_quantity)
main.test.FINAL$sales_quantity=log(main.test.FINAL$sales_quantity)

#main.train$outlet_code=as.numeric(substr(main.train$outlet_code,1,12))

#One Hot Encoding freezer status and outlet_region and expected rainfall category
library(caret)

dmy=dummyVars("~.",data=main.train)
main.train.FINAL=data.frame(predict(dmy,newdata = main.train))

dmy=dummyVars("~.",data=main.test)
main.test.FINAL=data.frame(predict(dmy,newdata = main.test))

dim(main.test.FINAL)
dim(main.train.FINAL)

#RIDGE AND LASSO
x.train=model.matrix(sales_quantity~.,main.train.FINAL)[,-1]
x.test=model.matrix(sales_quantity~.,main.test.FINAL)[,-1]
y.train=main.train.FINAL$sales_quantity
y.test=main.test.FINAL$sales_quantity

library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda = grid)

set.seed(1)
cv.out=cv.glmnet(x.train,y.train,alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx = x.test)
ridge.pred=as.numeric(ridge.pred)
#mean((ridge.pred - y.test)^2)
mean(abs(ridge.pred-y.test)/y.test)

#Lasso Regression
lasso.mod=glmnet(x.train,y.train,alpha = 1,lambda = grid)
plot(lasso.mod)

set.seed(1)
cv.out=cv.glmnet(x.train,y.train,alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx = x.test)
lasso.pred=as.numeric(lasso.pred)
#mean((ridge.pred - y.test)^2)
mean(abs(lasso.pred-y.test)/y.test)


#XGB 

library(gbm)
set.seed(1)
boost.model=gbm(sales_quantity ~.,
                data=main.train.FINAL,
                distribution = "gaussian",
                n.trees = 5000,
                interaction.depth = 4,shrinkage=0.3,verbose=F) #Fitting the gradient boosting model with our data

summary(boost.model) #Providing a summary of the boost model
plot(boost.model, i="outlet_code")

#predicting output for sales quantity
yhat.boost = predict(boost.model,newdata = main.test.FINAL, n.trees = 5000)#Predicting sales_quantity values by the test dataset
yhat.boost.exp=exp(yhat.boost)
y=main.test.tree.backup$sales_quantity

#calculating the MAPA value
MAPE.xgb=sum(abs(exp(yhat.boost) -y.test))/sum(y.test)
MAPE.xgb

#Random Forrest
library(randomForest)
set.seed(1)
rf=randomForest(sales_quantity ~.,data=main.train.FINAL,mtry=6,importance=TRUE)
yhat.rf=predict(rf,newdata=subset(main.test.FINAL,select = -sales_quantity))
yhat.rf=as.numeric(yhat.rf)
yhat.rf.exp=exp(yhat.rf)
MAPE.rf=sum(abs(exp(yhat.rf) -y.test))/sum(y.test)
MAPE.rf
