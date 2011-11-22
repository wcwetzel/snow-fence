# snow fence plants - gall fate
library(ggplot2)
library(bbmle)


d = read.csv('~/Documents/DATA/2010 DATA/LAB/Snow fence data/snow fences with gall data.csv')
#d = d[d$galls>0, ]

d$ptrate = d$ptized / d$galls
d$svrate = d$healthy / d$galls
d$umrate = d$unkmort / d$galls
d$pdrate = d$pred / d$galls


plot(d$ptrate ~ d$galls)



p1 = ggplot(data=d, aes(x = galls, y = ptrate)) +
	geom_point(alpha=0.3, position='jitter') +
	#stat_smooth(method='glm', family='binomial') +
	scale_x_continuous("Gall abundance", limits=c(1,11.5)) +
	scale_y_continuous('Parasitism rate') +
	stat_smooth(colour='green') + 
	theme_bw() + 
	opts( panel.grid.minor = theme_blank(), panel.grid.major = theme_blank(),
		axis.title.x = theme_text(vjust = 0))
print(p1)
ggsave('~/Documents/Analysis repos/snow-fence/figs/ptrate.pdf', p1, width=3.5, height=3)



m = lm(ptrate ~ galls, data=d)
summary(m)


m2 = mle2(ptized ~ dbinom(size=galls, prob=p), start=list(p=0.2), data=d)
m3 = mle2(ptized ~ dbinom(size=galls, prob=1 / (1 + exp(p + a * galls))), start=list(p=0.2, a=0), data=d)

AICtab(m2, m3)
anova(m2, m3)

p2 = ggplot(data=d, aes(x = galls, y = svrate)) +
	geom_point(alpha=0.3, position='jitter') +
	scale_x_continuous("Gall abundance", limits=c(1,11.5)) +
	scale_y_continuous('Survival rate') +
	#stat_smooth(method='glm', family='binomial') +
	stat_smooth(colour='green') +
	theme_bw() +
	opts( panel.grid.minor = theme_blank(), panel.grid.major = theme_blank(),
		axis.title.x = theme_text(vjust = 0))
print(p2)
ggsave('~/Documents/Analysis repos/snow-fence/figs/svrate.pdf', p2, width=3.5, height=3)


m4 = mle2(healthy ~ dbinom(size=galls, prob=p), start=list(p=0.2), data=d)
m5 = mle2(healthy ~ dbinom(size=galls, prob=1 / (1 + exp(p + a * galls))), start=list(p=0.2, a=0), data=d)

AICtab(m4, m5)
anova(m4, m5)

p3 = ggplot(data=d, aes(x = galls, y = pdrate)) +
	geom_point(alpha=0.3, position='jitter') +
	scale_x_continuous("Gall abundance", limits=c(1,11.5)) +
	scale_y_continuous('Predation rate') +
	theme_bw() +
	#stat_smooth(method='glm', family='binomial') +
	stat_smooth(colour='green') +
	opts( panel.grid.minor = theme_blank(), panel.grid.major = theme_blank(),
		axis.title.x = theme_text(vjust = 0))
print(p3)
ggsave('~/Documents/Analysis repos/snow-fence/figs/pdrate.pdf', p3, width=3.5, height=3)


m6 = mle2(pred ~ dbinom(size=galls, prob=p), start=list(p=0.2), data=d)
m7 = mle2(pred ~ dbinom(size=galls, prob=1 / (1 + exp(p + a * galls))), start=list(p=0.2, a=0), data=d)

AICtab(m6, m7)
anova(m6, m7)

pm7 = predict(m7, newdata=data.frame(galls= 0:11))
plot(pred ~ galls, data=d)
points(pm7 ~ I(0:11), data=d, type='l')

p4 = ggplot(data=d, aes(x = galls, y = umrate)) +
	geom_point(alpha=0.3, position='jitter') +
	scale_x_continuous("Gall abundance", limits=c(1,11.5)) +
	scale_y_continuous('Unknown mortality') +
	theme_bw() +
	stat_smooth(colour='green') +
	opts( panel.grid.minor = theme_blank(), panel.grid.major = theme_blank(),
		axis.title.x = theme_text(vjust = 0))
print(p4)
ggsave('~/Documents/Analysis repos/snow-fence/figs/umrate.pdf', p4, width=3.5, height=3)

m8 = mle2(unkmort ~ dbinom(size=galls, prob=p), start=list(p=0.2), data=d)
m9 = mle2(unkmort ~ dbinom(size=galls, prob=1 / (1 + exp(p + a * galls))), start=list(p=0.2, a=0), data=d)

AICtab(m8, m9)
anova(m9, m8)


p5 = ggplot(data=d, aes(x = galls, y = ptrate + pdrate)) +
	geom_point(alpha=0.3, position='jitter') +
	#stat_smooth(method='glm', family='binomial') +
	stat_smooth()
print(p5)

m8 = mle2(unkmort ~ dbinom(size=galls, prob=p), start=list(p=0.2), data=d)
m9 = mle2(unkmort ~ dbinom(size=galls, prob=1 / (1 + exp(p + a * galls))), start=list(p=0.2, a=0), data=d)

AICtab(m8, m9)




################################
## actual snow fence analysis ##

d$M[d$transect=='M'] = 1
d$M[d$transect!='M'] = 0
d$AM[d$transect=='AM'] = 1
d$AM[d$transect!='AM'] = 0
d$P[d$transect=='P'] = 1
d$P[d$transect!='P'] = 0

s1 = mle2(galls ~ dnbinom(mu = mu, size=s), start=list(mu = 2, s = 1), data=d)
s2 = mle2(galls ~ dnbinom(mu = am + m * M + p * P, size=s), 
	start=list(am = 2, m = 0, p = 0, s = 1), data=d)
s3 = mle2(galls ~ dnbinom(mu = mu + am * AM + m * M + p * P, size=s), 
	start=list(mu=3, am = 0, m = 0, p = 0, s = 1), data=d)
s4 = mle2(galls ~ dnbinom(mu = am + trt * pmax(M,P), size=s), 
	start=list(am = 2, trt = -1, s = 1), data=d)

AICtab(s1, s2, s3, s4)


plot(galls ~ transect, data=d)
plot(galls ~ as.factor(AM), data=d)


sf1 = mle2(field_galls ~ dnbinom(mu = mu, size=s), start=list(mu = 2, s = 1), data=d)
sf2 = mle2(field_galls ~ dnbinom(mu = am + m * M + p * P, size=s), 
	start=list(am = 2, m = 0, p = 0, s = 1), data=d)
sf3 = mle2(field_galls ~ dnbinom(mu = mu + am * AM + m * M + p * P, size=s), 
	start=list(mu=3, am = 0, m = 0, p = 0, s = 1), data=d)
AICtab(sf1, sf2, sf3)
plot(field_galls ~ transect, data=d)




