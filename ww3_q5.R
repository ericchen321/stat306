flighttime=c(1, 2, 1.3, 1.69, 1.86, 1.89, 1.73, 1.85, 1.25, 1.94, 1.1, 1.98, 0.93, 1.87, 1.41, 1.61, 1.94, 2.08, 1.21, 1.83, 1.87, 1.98, 1.82, 1.43, 0.93) 
len=c(6.6, 6.6, 6.1, 5.6, 6.6, 6.1, 6.1, 6.6, 5.6, 6.6, 5.6, 6.6, 6.6, 5.6, 6.1, 5.6, 6.1, 6.6, 5.6, 5.6, 6.1, 5.6, 6.1, 6.1, 6.6) 
wid=c(0.7, 2.1, 0.7, 2.1, 2.1, 1.4, 1.4, 1.4, 0.7, 1.4, 0.7, 1.4, 0.7, 1.4, 0.7, 2.1, 2.1, 2.1, 0.7, 1.4, 1.4, 1.4, 2.1, 0.7, 0.7) 
len.centered=len-6
wid.centered=wid-1.5

regModel1=lm(flighttime ~ len + wid + I(len^2) + I(wid^2) + len*wid)
regModel2=lm(flighttime ~ len.centered + wid.centered + I(len.centered^2) + I(wid.centered^2) + len.centered*wid.centered)

summary(regModel1)
summary(regModel2)