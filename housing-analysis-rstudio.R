Sys.time()

# Install packages and load libraries
if (!require("pacman")) install.packages("pacman")

pacman::p_load(ggplot2, dplyr, broom, ggpubr, tidyverse, psych, readr, viridis)

library(tidyverse)
library(psych)
library(readr)
library(viridis)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

# Read training and testing data sets into RStudio
setwd("D:/R")
housesTrain<- read_csv("training.csv")
housesTest <- read_csv("testing.csv")

Sys.time()
# Summary of the Sale Price in  the test data set
summary(housesTest$SalePrice)
summary(housesTrain$SalePrice)

Sys.time()
# Plot histogram of sales price for test data set
options(scipen=999)
hist(housesTest$SalePrice, labels=TRUE, main="Distribution of Sale Price in Testing Data",
     xlab= "Sale Price", col="purple")
par(new=TRUE)
boxplot(housesTest$SalePrice, horizontal = TRUE, axes=FALSE,col="yellow")
box()

Sys.time()
# Combine training and testing files
Combo <- combine(housesTest, housesTrain)

# Plot histogram of Sale Price in combined set
hist(Combo$SalePrice, labels=TRUE, main="Distribution of Sale Price in Combined Data",
     xlab= "Sale Price", col="purple")
par(new=TRUE)
boxplot(Combo$SalePrice, horizontal = TRUE, axes=FALSE,col="yellow")
box()

Sys.time()
# Linear regression model on training set
fit <- lm(SalePrice ~., data=housesTrain)
summary(fit)

Sys.time()
# Remove all the rows with missing values in Testing data set
housesTest <- na.omit(housesTest)
# Read only the first 20 rows from test data to predict
test20 <- housesTest[1:20,]
# Predict the sale price
predict_price <- predict(fit, newdata=test20)
tidy(predict_price)

Sys.time()
# Compare predicted to actual prices
df1 <- data.frame(x=1:20, actual=test20$SalePrice, predicted=predict_price)
df1
plot(df1$x, df1$actual, main="Actual vs. Predicted Sales Prices", type = "b", 
     pch=19, col = "red", xlab="x", ylab="Prices", ylim=c(42000,290000))
lines(df1$x, df1$predicted, type = "b", pch=18, col = "blue", lty=1)
legend("topright", legend=c("Actual Prices", "Predicted Prices"), col=c("red", "blue"), 
       lty=1, cex=0.8)


# Clear Environment and Graphics
rm(list=ls())
graphics.off()
