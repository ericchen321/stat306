# workspace setup
#install.packages("leaps")
#library(leaps)
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

# cross-validation selection: TODO

# now we've decided to select fro_num as the only explanatory variable.
# let's see if model1 yields a good residual plot
plot(fro_num, model1$residuals)

# the plot is bad, so we eyeball the relationship btw res and fro_num
plot(fro_num, res)

# now we build models of higher order
model1Quad<-lm(res~poly(fro_num,2,raw=TRUE),data=data)
model1Cubic<-lm(res~poly(fro_num,3,raw=TRUE),data=data)
model1Pwr4<-lm(res~poly(fro_num,4,raw=TRUE),data=data)
model1Pwr5<-lm(res~poly(fro_num,5,raw=TRUE),data=data)

# residual plots of higher-order models
plot(fro_num, model1Quad$residuals)
plot(fro_num, model1Cubic$residuals)
plot(fro_num, model1Pwr4$residuals)
plot(fro_num, model1Pwr5$residuals)

# we notice 4th and 5th order models have good residual plots. We then
# decide between them using the p-values of their coefficients, and
# their adjusted R^2
summary(model1Pwr4)
summary(model1Pwr5)

# so we decide to use the 4th power model