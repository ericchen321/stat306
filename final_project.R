# workspace setup
#install.packages("leaps")
library(leaps)
rm(list = ls())
setwd("C:/users/cgx19/stat306")
data <- read.table(file = "yacht_hydrodynamics.data", header = FALSE)
names(data)<-c("pos", "pris_coe", "len_dis_rat", "be_dr_rat", "len_be_rat", "fro_num", "res")

# variable selection
# subset selection
sSubset<-regsubsets(res~.,data=data,method="exhaustive")
ssSubset<-summary(sSubset)
model1<-lm(res~fro_num, data=data)
model2<-lm(res~pris_coe+fro_num, data=data)
model3<-lm(res~pos+pris_coe+fro_num, data=data)
model4<-lm(res~pos+pris_coe+len_dis_rat+fro_num, data=data)
model5<-lm(res~pos+len_dis_rat+be_dr_rat+len_be_rat+fro_num, data=data)
model6<-lm(res~.,data=data)
models<-c(model1,model2,model3,model4,model5,model6)
aics<-c(AIC(model1,k=2),AIC(model2,k=3),AIC(model3,k=4),AIC(model4,k=5),AIC(model5,k=6),AIC(model6,k=7))

# forward selection
sForward<-regsubsets(res~.,data=data,method="forward")
ssForward<-summary(sForward)

# TODO: cross-validation selection, but maybe not necessary?

# now we've decided to select fro_num as the only explanatory variable,
# due to the following resaons:
# 1) for every best model with 1/2/../6 variables, only fro_num yields
#    significant coefficients
# 2) The res~fro_num model has the lowest adj. R^2
# 3) The res~fro_num model has the lowest AIC value
# now we want to get the the residual plot of the res~fro_num model
# to see if the model is fine
plot(data$fro_num, model1$residuals)

# the plot is bad, so we eyeball the relationship btw res and fro_num,
# to see if we can tweak their relationship
plot(data$fro_num, data$res)

# we noticed two things from the res vs. fro_num plot:
# 1) res becomes more spread out as fro_num increases
# 2) res increases faster than linear rate
# so we decide to address what we've noticed by
# 1) applying log to res, and
# 2) raise fro_num to higher powers

# now we build a few models of the form log(res)~fro_num^<power>
resLog<-log(data$res)

# we do variable selection again to see if any other explanatory variables
# should be included
sSubsetLog<-regsubsets(data$res~.,data=data,method="exhaustive")
ssSubsetLog<-summary(sSubset)

# now we've verified that stil we should include fro_num only;
# we can proceed to build linear models
model1Log<-lm(resLog~poly(data$fro_num,1,raw=TRUE),data=data)
model1LogQuad<-lm(resLog~poly(data$fro_num,2,raw=TRUE),data=data)
model1LogCubic<-lm(resLog~poly(data$fro_num,3,raw=TRUE),data=data)
model1LogPwr4<-lm(resLog~poly(data$fro_num,4,raw=TRUE),data=data)
model1LogPwr5<-lm(resLog~poly(data$fro_num,5,raw=TRUE),data=data)

# and plot their residuals
plot(data$fro_num, model1Log$residuals)
plot(data$fro_num, model1LogQuad$residuals)
plot(data$fro_num, model1LogCubic$residuals)
plot(data$fro_num, model1LogPwr4$residuals)
plot(data$fro_num, model1LogPwr5$residuals)

# we notice the 4th and 5th power model yield good residual plots;
# so we use summary() to compare them using p-values of coefficients
# and adj R^2 values
summary(model1LogPwr4)
summary(model1LogPwr5)

# we find that the 5th power model has insignificant coefficients,
# and lower adj R^2. So we use the 4th power model;
# also we do the qq-plot to make sure the residual satisifies the
# normality assumption
qqnorm(model1LogPwr4$residuals)
qqline(model1LogPwr4$residuals)

# inspect the AIC value of our final value
AIC(model1LogPwr4,k=5)

# what if we include pris_coe as well?
model1LogWithPrisCoe<-lm(resLog~poly(data$fro_num,1,raw=TRUE)+data$pris_coe,data=data)
model1LogQuadWithPrisCoe<-lm(resLog~poly(data$fro_num,2,raw=TRUE)+data$pris_coe,data=data)
model1LogCubicWithPrisCoe<-lm(resLog~poly(data$fro_num,3,raw=TRUE)+data$pris_coe,data=data)
model1LogPwr4WithPrisCoe<-lm(resLog~poly(data$fro_num,4,raw=TRUE)+data$pris_coe,data=data)
model1LogPwr5WithPrisCoe<-lm(resLog~poly(data$fro_num,5,raw=TRUE)+data$pris_coe,data=data)
