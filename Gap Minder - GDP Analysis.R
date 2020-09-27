file1<- read.csv('C:/Users/dellpc/Downloads/gapminder.csv', header=TRUE)
summary(file1)

#install.packages('mice')
library('mice')
md.pattern(file1)
#Removing the not required countries
#install.packages('dplyr')
#install.packages('stringr')
library(dplyr)
library(stringr)
file2 <- file1 %>%
  filter(!(str_detect(Country,"Ã.land")))
file3 <- file2%>%
  filter(!(Country %in% c("Central African Rep.","Czech Rep.","Dominican Rep.","Korea, Dem. Rep.","Korea, Rep.","Kyrgyzstan","Laos","Netherlands Antilles","Saint Lucia", "Saint Vincent and the Grenadines","Tokelau","Yemen, Rep.")))
md.pattern(file3)

#finding the average values for each country and adding those columns in file5
#install.packages('tidyverse')
library(tidyverse)
file4 <- file3 %>%
  group_by(Country) %>%
  summarise(Average_mortality = mean(child_mortality,na.rm = TRUE), Average_gdp = mean(gdp, na.rm = TRUE))
file5 <- merge(file3,file4, by = "Country")  
#Putting zero value for countries in which values are missing completely
file5$child_mortality[is.na(file5$Average_mortality)] <- 0
file5$Average_gdp[is.na(file5$Average_gdp)] <- 0
file5$child_mortality[is.nan(file5$Average_mortality)] <- 0
file5$Average_gdp[is.nan(file5$Average_gdp)] <- 0
View(file5)
file5$child_mortality <- ifelse(is.na(file5$Average_mortality),file5$Average_mortality, file5$child_mortality)
file5$gdp <- ifelse(is.na(file5$gdp),file5$Average_gdp, file5$gdp)
df <- file5
df<-read.csv("G:/Data mining/week4/cd.csv")
df<-df[,1:9]
#Performing Linear Regression
gdp_reg<-lm(df$gdp~df$fertility+df$life+df$population+df$child_mortality,data=df)
summary(gdp_reg)
#Getting a scatter plot of all independent variables with dependent variable
par(mfrow=c(2,2))
plot(df$fertility, df$gdp)
plot(df$life, df$gdp)
plot(df$population, df$gdp)
plot(df$child_mortality, df$gdp)
# Predicting the GDP values with the model
prediction<-as.data.frame(predict(gdp_reg,new_data=df))
df1<-df
df1[, 10]<-prediction

#Error Calculation
df1[, 11] <- df1[, 10] - df1[, 8]
df1[, 12] <- df1[, 11]**2
#View(df1)
MSE <- mean(df1[, 12])
#Naming the column in the dataset
names(df1)<-c("ID", "Country" , "Year" , "Fertility_Rates" , "Life_Expectancy" , "Population" , "Child_Mortality_Rates" , "Observed_GDP" , "Region" , "Predicted_GDP" , "Error" , "Error^2")
# Dividing in test and train dataset 
library(rsample)  # data splitting 
library(glmnet)   # implementing regularized regression approaches
library(dplyr)    # basic data manipulation procedures
library(ggplot2)  
df<-read.csv("G:/Data mining/week4/cd.csv")
data <- df[c('fertility','life','population','child_mortality','gdp')]
d_div<-sample(2,nrow(data),replace=T,prob = c(0.6,0.4))

d_test<-data[d_div==1,]
d_train<-data[d_div==2,]
x_test <- model.matrix(gdp~.,d_test)[,-1]
y_test <- d_test$gdp
x_train<- model.matrix(gdp~.,d_train)[,-1]
y_train<-d_train$gdp
#Giving a range to the lambda value
lambda <- 10^seq(10, -2, length = 100)
#Ridge Regression
ridge.mod <- glmnet(x_train,y_train, alpha = 0, lambda = lambda)
predict(ridge.mod, s = 0, type = 'coefficients')
summary(ridge.mod)
plot(ridge.mod)
#Cross-validating the Ridge model 
model_ridge <- cv.glmnet(x_train,y_train,alpha = 0)
summary(model_ridge)
plot(model_ridge)
#Ridge fit
ridge_fit = glmnet(x_train,y_train,alpha = 0, lambda = model_ridge$lambda.1se)
ridge_fit
names(ridge_fit)
ridge_fit$beta[,1]
summary(ridge_fit)
#Ridge Prediction
opt_ridge_lambda <- ridge_fit$lambda
y_ridge_predicted <- predict(ridge_fit, s = opt_ridge_lambda, newx = x_train)

# Sum of squares Total & Error - Ridge 
sst_ridge <- sum((y_train-mean(y_train))^2)
sse_ridge <- sum((y_ridge_predicted - y_train)^2)
rsq_ridge <- 1 - sse_ridge/sst_ridge
rsq_ridge



