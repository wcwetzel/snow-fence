### Photosynthesis - snowfence analysis ###
## 11 Apr 2012 ##

library(ggplot2)
library(bbmle)


d = read.csv('~/Documents/DATA/2010 DATA/LAB/Snow fence data/snow fences with gall data.csv')
#d = d[d$galls>0, ]

d$area = d$d1 * d$d2

d$gdens = d$galls / d$area

d$ptrate = d$ptized / d$galls
d$svrate = d$healthy / d$galls
d$umrate = d$unkmort / d$galls
d$pdrate = d$pred / d$galls

d$photo = apply(cbind(d$photo1, d$photo2, d$photo3), 1, mean)


ggplot(data=d, aes(x=photo, y=galls)) + geom_point() + 
	stat_smooth() + stat_smooth(method='lm')
	
ggplot(data=d, aes(x=photo, y=gdens)) + geom_point() + 
	stat_smooth() + stat_smooth(method='lm')

ggplot(data=d, aes(x=photo, y=healthy)) + geom_point() + 
	stat_smooth() + stat_smooth(method='lm')

ggplot(data=d, aes(x=photo, y=ptized)) + geom_point() + 
	stat_smooth() + stat_smooth(method='lm')

ggplot(data=d, aes(x=photo, y=pred)) + geom_point() + 
	stat_smooth() + stat_smooth(method='lm')

ggplot(data=d, aes(x=photo, y=unkmort)) + geom_point() + 
	stat_smooth() + stat_smooth(method='lm')

ggplot(data=d, aes(x=photo, y=ptrate)) + geom_point() + 
	stat_smooth() + stat_smooth(method='lm')

ggplot(data=d, aes(x=photo, y=svrate)) + geom_point() + 
	stat_smooth() + stat_smooth(method='lm')

ggplot(data=d, aes(x=photo, y=umrate)) + geom_point() + 
	stat_smooth() + stat_smooth(method='lm')

ggplot(data=d, aes(x=photo, y=pdrate)) + geom_point() + 
	stat_smooth() + stat_smooth(method='lm')

m0 = lm(gdens ~ 1, data=d)
m1 = lm(gdens ~ photo, data=d)

AICtab(m0,m1)