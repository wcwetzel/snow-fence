# grid of all relationships for snow fence gall fates #
# 5 May 2012



d = read.csv('~/Documents/DATA/2010 DATA/LAB/Snow fence data/snow fences with gall data.csv')

d = d[d$galls>0,]

d$ptism.rate = d$ptized / d$galls
d$s = d$healthy / d$galls
d$unk.rate = d$unkmort / d$galls
d$pred.rate = d$pred / d$galls



data2 = d




#------- plot a grid of all relationships -------------------#

par(mfcol = c(2, 4))

plot(s + runif(nrow(data2), -0.03,0.03) ~ I(galls + runif(nrow(data2), -0.2,0.2)), data=data2, 
	pch=20, ylab='Survival rate', xlab='Galls')
lines(lowess(data2$s ~ data2$galls), type='l')
plot(s + runif(nrow(data2), -0.03,0.03) ~ log(galls + runif(nrow(data2), 0, 0.05)), 
	data=data2, pch=20,  ylab='Survival rate', xlab='log(Galls)')
lines(lowess(data2$s ~ log(data2$galls)), type='l')

plot(ptism.rate + runif(nrow(data2), -0.03,0.03)  ~ I(galls + runif(nrow(data2), -0.2,0.2)), 
	data=data2, pch=20, ylab='Parasitism rate', xlab='Galls')
lines(lowess(data2$ptism.rate ~ data2$galls), type='l')
plot(ptism.rate + runif(nrow(data2), -0.03,0.03) ~ log(galls + runif(nrow(data2), 0, 0.05)), 
	data=data2, pch=20, ylab='Parasitism rate', xlab='log(Galls)')
lines(lowess(data2$ptism.rate ~ log(data2$galls)), type='l')

plot(pred.rate + runif(nrow(data2), -0.03,0.03) ~ I(galls + runif(nrow(data2), -0.2,0.2)), 
	data=data2, pch=20, las=1, ylab='Predation rate', xlab='Galls')
lines(lowess(data2$pred.rate ~ data2$galls), type='l')
plot(pred.rate + runif(nrow(data2), -0.03,0.03) ~ log(galls + runif(nrow(data2), 0, 0.05)),
	data=data2, pch=20,  ylab='Predation rate', xlab='log(Galls)')
lines(lowess(data2$pred.rate ~ log(data2$galls)), type='l')

plot(unk.rate + runif(nrow(data2), -0.03,0.03) ~ I(galls + runif(nrow(data2), -0.2,0.2)), data=data2, 
	pch=20, ylab='Unk mortality rate', xlab='Galls')
lines(lowess(data2$unk.rate ~ data2$galls), type='l')
plot(unk.rate + runif(nrow(data2), -0.03,0.03) ~ log(galls + runif(nrow(data2), 0, 0.05)), data=data2, 
	pch=20,  ylab='Unk mortality rate', xlab='log(Galls)')
lines(lowess(data2$unk.rate ~ log(data2$galls)), type='l')

