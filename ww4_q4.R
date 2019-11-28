rm(list = ls())

# response variable: asking price divided by 10000
askpr=c(54.8, 45.99, 58.39, 57.8, 68.5, 73.8, 48.8, 62.9, 68.8, 59.8, 48.5, 68.5, 50.5, 51.68, 81.9, 25.9, 62.8888, 74.8, 52.4, 57.8, 47.8, 78.8, 54.8, 50.8, 57.5, 53.8, 71.99, 60.8, 56.8, 50.8, 46.8, 53.9, 44.8, 58.8, 40.9, 65.99, 40.8, 26.99, 33.7, 55.2) 
# explanatory variables:
# (i) finished floor area divided by 100
ffarea=c(11.26, 16.01, 15.09, 13.84, 13.59, 17.54, 14.8, 14, 16.9, 17.63, 14.8, 15.76, 12.26, 15.1, 20.95, 6.1, 15.77, 17.48, 16.22, 12.01, 13.34, 19.48, 15.46, 16.6, 13.46, 12.22, 15.05, 13.2, 15.5, 12.27, 16.2, 11.84, 9.4, 17.37, 16.06, 22.78, 14, 10.5, 12, 15.3) 
# (ii) age
age=c(0, 25, 8, 10, 2, 9, 50, 5, 8, 26, 24, 4, 3, 20, 19, 11, 6, 5, 25, 0, 32, 11, 41, 23, 10, 9, 8, 3, 23, 17, 30, 15, 14, 26, 25, 35, 38, 37, 28, 9) 

richmondtownh=data.frame(askpr,ffarea,age)

# a
regModel=lm(askpr~ffarea+age)
summary(regModel)

# b
askpr_fitted=unname(regModel$fitted.values[1:40])
mean(askpr)
sd(askpr)
mean(askpr_fitted)
sd(askpr_fitted)

# c
cov(askpr, askpr_fitted)
cor(askpr, askpr_fitted)

# e
w=20.1303+3.35584*ffarea-0.801228*age
cor(askpr, w)