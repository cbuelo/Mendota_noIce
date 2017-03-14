#Script to calc probabilities of no ice cover on Mendota
#CDB, 3/14/17

#read in data, scraped from http://www.aos.wisc.edu/~sco/lakes/Mendota-ice.html
dat = read.csv("MendotaIce_1855_2016.csv", stringsAsFactors=FALSE)

#plot time series of cover
plot(dat[,c("Winter_start", "DAYS")])

#linear model for days of ice cover
ice_lm = lm(DAYS~Winter_start, data=dat)
summary(ice_lm)
abline(ice_lm)

#plot extended
plot(dat[,c("Winter_start", "DAYS")], xlim=c(1850, 2500), ylim=c(0, 165))
abline(ice_lm)
abline(h=0)

#sd of scatter around trend
SD = sd(resid(ice_lm))

# 'probability' of no ice in a given year (assume linear trend and constant scatter around trend)
prob_noIce = data.frame(Winter_start=1850:2500)
prob_noIce$Cover_trend = predict(ice_lm, prob_noIce)
prob_noIce$P_noIce_thisYear = pnorm(-1*prob_noIce$Cover_trend,mean=prob_noIce$Cover_trend, sd=SD)
prob_noIce$P_yesIce_thisYear = 1 - prob_noIce$P_noIce_thisYear

#probability of continuous ice up to a given year (ignoring that we have observations up to 2016, probably okay as those probabilities are essentially 1)
P_alwaysIce_toThisYear_inclusive = cumprod(prob_noIce$P_yesIce_thisYear)
P_alwaysIce_beforeThisYear = c(1,P_alwaysIce_toThisYear_inclusive[-length(P_alwaysIce_toThisYear_inclusive)])
prob_noIce$P_alwaysIce_beforeThisYear = P_alwaysIce_beforeThisYear

#probability that a given year is the first w/o ice cover
prob_noIce$P_thisYear_firstNoIce = prob_noIce$P_alwaysIce_beforeThisYear * prob_noIce$P_noIce_thisYear

#plot
png("Mendota_iceCover.png")
par(mfrow=c(4,1), mar=c(1.5,4.5,1,1), oma=c(3,0.5,0,0), cex.lab=1.3)
plot(prob_noIce[c("Winter_start", "Cover_trend")], type="l", ylim=c(-10,165), ylab="Ice cover, days")
abline(h=0)
points(dat[,c("Winter_start", "DAYS")])
plot(prob_noIce[c("Winter_start", "P_noIce_thisYear")], ylab="P(no ice this year)")
plot(prob_noIce[c("Winter_start", "P_alwaysIce_beforeThisYear")], ylab="P(always ice before)")
plot(prob_noIce[c("Winter_start", "P_thisYear_firstNoIce")], ylab="P(first year no ice)")
mtext("Year", side=1, outer=TRUE, line=1.5)
dev.off()

#best guess
prob_noIce[which.max(prob_noIce$P_thisYear_firstNoIce), "Winter_start"]
max(prob_noIce$P_thisYear_firstNoIce)