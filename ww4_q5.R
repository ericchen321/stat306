rm(list = ls())

ls.cvrmse <- function(ls.out)
  # Compute cross-validated root mean squared error of prediction.
  # Handles missing values.
  # ls.out is a fitted regression model from lsreg or lm.
  # (c) Copyright William J. Welch 1997
  # 2005.04.02: arguments x, y, intercept replaced by ls.out.
{
  res.cv <- ls.out$residuals / (1.0 - ls.diag(ls.out)$hat)
  
  # Identify NA's and remove them.
  is.na.res <- is.na(res.cv)
  res.cv <- res.cv[!is.na.res]
  
  cvrmse <- sqrt(sum(res.cv^2) / length(res.cv))
  return(cvrmse)
}

# asking price divided by 10000:
askpr=c(45.99, 78.8, 51.99, 40.8, 40.9, 53.8, 58.68, 53.8, 86.8, 62.8888, 46.8, 57.5, 44.8, 52.4, 54.8, 61.5, 79.8, 56.88, 108.8, 49.9, 56.8, 79.99, 53.9, 71.99, 50.5, 25.9, 62.9, 50.8, 65.99, 65.8, 54.98, 74.8, 57.8, 73.8, 41.99, 73.9, 48.8, 50.8, 57.8, 68.5, 47.8, 68.8, 58.8, 60.8, 51.68, 68.8, 81.9, 48.5, 26.99, 68.5) 

# The explanatory variables are:
# (i) finished floor area divided by 100
ffarea=c(16.01, 19.48, 12.09, 12.26, 16.06, 10.95, 13.96, 12.22, 15.08, 15.77, 16.2, 13.46, 9.4, 16.22, 11.26, 14.5, 15.25, 15.78, 23.98, 15.6, 15.5, 22, 11.84, 15.05, 12.26, 6.1, 14, 12.27, 22.78, 13.45, 13.06, 17.48, 12.01, 17.54, 12.9, 15.15, 14.8, 16.6, 13.84, 15.76, 13.34, 16.9, 17.37, 13.2, 15.1, 15.95, 20.95, 14.8, 10.5, 13.59)
# (ii) age
age=c(25, 11, 7, 29, 25, 18, 9, 9, 1, 6, 30, 10, 14, 25, 0, 7, 3, 17, 16, 20, 23, 20, 15, 8, 3, 11, 5, 17, 35, 1, 1, 5, 0, 9, 44, 0, 50, 23, 10, 4, 32, 8, 26, 3, 20, 18, 19, 24, 37, 2)
# (iii) monthly maintenance fee divided by 10
mfee=c(33.7, 20.4, 18.1, 19.8, 24.4, 24.7, 22, 18.5, 48.8, 35.7, 16, 22.1, 23.3, 36.4, 24.8, 18.7, 35, 17.3, 36.9, 27, 17.4, 26.7, 21, 22.3, 18, 17.1, 19.6, 25.2, 57.4, 18.2, 19.6, 29.7, 14.2, 18.2, 23.2, 22.2, 25, 19.9, 16, 22.1, 24.5, 19.4, 31, 18.9, 24.5, 23.6, 34.8, 16.1, 28, 17)
# (iv) number of bedrooms
beds=c(3, 3, 3, 3, 2, 2, 3, 3, 3, 3, 4, 3, 2, 3, 2, 3, 2, 4, 3, 3, 3, 3, 2, 3, 3, 1, 3, 2, 2, 3, 3, 4, 3, 4, 3, 4, 3, 4, 3, 4, 3, 4, 3, 3, 3, 3, 1, 3, 2, 3) 
# After you have copied the above R vectors into your R session, you can get a dataframe with 
richmondtownh=data.frame(askpr,ffarea,age,mfee,beds) 

# The corresponding vectors for the holdout set are:
askpr.ho=c(33.7, 77.8, 47.9, 40.8, 55.8, 55.2, 54.8, 59.8, 58.39)
ffarea.ho=c(12, 16.5, 12.1, 14, 13.06, 15.3, 15.46, 17.63, 15.09)
age.ho=c(28, 3, 7, 38, 0, 9, 41, 26, 8)
mfee.ho=c(25.9, 25.4, 18, 23, 18.6, 16.9, 31, 32, 20.3)
beds.ho=c(2, 4, 3, 3, 3, 3, 3, 5, 4) 
# Create a second data frame: 
holdout=data.frame(askpr.ho,ffarea.ho,age.ho,mfee.ho,beds.ho)
names(holdout)=names(richmondtownh) # [to make variables names the same as before] 

# a
regModel3Vars=lm(askpr~ffarea+age+mfee, data=richmondtownh)
regModel4Vars=lm(askpr~., data=richmondtownh)
summary(regModel3Vars)
summary(regModel4Vars)

# b
cvrmse3VarsLeaveOneOut=ls.cvrmse(regModel3Vars)
cvrmse4VarsLeaveOneOut=ls.cvrmse(regModel4Vars)

# c
fittedVals3Vars=predict(regModel3Vars, holdout)
hormse3Vars=sqrt(sum((askpr.ho - fittedVals3Vars)^2)/length(askpr.ho))
fittedVals4Vars=predict(regModel4Vars, holdout)
hormse4Vars=sqrt(sum((askpr.ho - fittedVals4Vars)^2)/length(askpr.ho))

# d
