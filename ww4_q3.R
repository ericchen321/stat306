rm(list = ls())

# response variable: asking price divided by 10000
askpr=c(65.99, 81.9, 68.8, 62.9, 26.99, 40.8, 57.8, 25.9, 50.5, 77.8, 79.99, 56.88, 79.8, 50.8, 55.2, 54.98, 52.4, 56.8, 62.8888, 53.8, 57.5, 47.8, 68.5, 71.99, 48.5, 58.39, 53.9, 53.8, 59.8, 61.5, 45.99, 55.8, 58.68, 51.99, 73.9, 48.8, 51.68, 40.9, 68.8, 40.8, 58.8, 46.8, 50.8, 41.99, 44.8, 49.9, 65.8, 68.5, 33.7, 54.8)
# explanatory variables:
# (i) finished floor area divided by 100
ffarea=c(22.78, 20.95, 15.95, 14, 10.5, 14, 13.84, 6.1, 12.26, 16.5, 22, 15.78, 15.25, 16.6, 15.3, 13.06, 16.22, 15.5, 15.77, 10.95, 13.46, 13.34, 15.76, 15.05, 14.8, 15.09, 11.84, 12.22, 17.63, 14.5, 16.01, 13.06, 13.96, 12.09, 15.15, 14.8, 15.1, 16.06, 16.9, 12.26, 17.37, 16.2, 12.27, 12.9, 9.4, 15.6, 13.45, 13.59, 12, 11.26)
# (ii) age
age=c(35, 19, 18, 5, 37, 38, 10, 11, 3, 3, 20, 17, 3, 23, 9, 1, 25, 23, 6, 18, 10, 32, 4, 8, 24, 8, 15, 9, 26, 7, 25, 0, 9, 7, 0, 50, 20, 25, 8, 29, 26, 30, 17, 44, 14, 20, 1, 2, 28, 0)
# (iii) monthly maintenance fee divided by 10
mfee=c(57.4, 34.8, 23.6, 19.6, 28, 23, 16, 17.1, 18, 25.4, 26.7, 17.3, 35, 19.9, 16.9, 19.6, 36.4, 17.4, 35.7, 24.7, 22.1, 24.5, 22.1, 22.3, 16.1, 20.3, 21, 18.5, 32, 18.7, 33.7, 18.6, 22, 18.1, 22.2, 25, 24.5, 24.4, 19.4, 19.8, 31, 16, 25.2, 23.2, 23.3, 27, 18.2, 17, 25.9, 24.8)
# (iv) number of bedrooms
beds=c(2, 1, 3, 3, 2, 3, 3, 1, 3, 4, 3, 4, 2, 4, 3, 3, 3, 3, 3, 2, 3, 3, 4, 3, 3, 4, 2, 3, 5, 3, 3, 3, 3, 3, 4, 3, 3, 2, 4, 3, 3, 4, 2, 3, 2, 3, 3, 3, 2, 2)
# (v) number of bathrooms
baths=c(3.5, 3.5, 3.5, 3.5, 1.5, 2.5, 3.5, 1.5, 3.5, 4.5, 4.5, 3.5, 3.5, 3.5, 3.5, 3.5, 2.5, 3.5, 3.5, 2.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 2.5, 3.5, 2.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 2.5, 2.5, 3.5, 4.5, 2.5, 3.5, 4.5, 2.5, 2.5, 2.5, 3.5, 3.5, 3.5, 2.5, 3.5)

richmondtownh=data.frame(askpr,ffarea,age,mfee,beds,baths)

library(leaps)

# a
sSubset=regsubsets(askpr~., data=richmondtownh, method="exhaustive")
ssSubset<-summary(sSubset)
