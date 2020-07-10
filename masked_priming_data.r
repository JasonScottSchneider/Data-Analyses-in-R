#Analyze the reaction times ('rt') and error rates (with reference to the 'error' column) for high-frequency (HF) and low-frequency (LF) words in the 'masked priming.csv' data set. Specifically, you should determine whether there is a statistically reliable difference between the reaction times (rt) and error rates for high-frequency (HF) and low-frequency (LF) words. For the reaction time analyses, analyze the data only for words that were correctly identified (i.e., with '0' in the 'error' column). Remember to do both by-subjects and by-items analyses. Also make sure that you run the appropriate statistical test for each of these analyses (and that you check the assumptions for each test).
	#Generate descriptive statistics.
	#Create a representative graph.
	#Write a short report of your findings.

mp=read.csv("masked priming.csv", header=TRUE)
mp.cor=mp[mp$error=="0",]
mp.t1 = aggregate(mp.cor$rt, list(mp.cor$subj, mp.cor$freq), mean)
mp.t2 = aggregate(mp.cor$rt, list(mp.cor$itemN, mp.cor$freq), mean)
colnames(mp.t1) = c("subj", "freq", "MeanRT")
colnames(mp.t2) = c("itemN", "freq", "MeanRT")
mp.t1$subj=as.factor(mp.t1$subj)
mp.t2$itemN=as.factor(mp.t2$itemN)
mp.t1.mean=tapply(mp.t1$MeanRT, mp.t1$freq, mean)
mp.t1.sd=tapply(mp.t1$MeanRT, mp.t1$freq, sd)
mp.t1.median=tapply(mp.t1$MeanRT, mp.t1$freq, median)
mp.t1.IQR=tapply(mp.t1$MeanRT, mp.t1$freq, IQR)
mp.t1.mean
mp.t1.sd
mp.t1.median
mp.t1.IQR
boxplot(mp.t1$MeanRT[mp.t1$freq=="HF"]-mp.t1$MeanRT[mp.t1$freq=="LF"], notch=T, ylab="", ylim=c(-100, 100), main="Mean RT difference (in ms) between HF and LF words (by subjects)")
abline(h=0, lty=2,col="grey")
hist((mp.t1$MeanRT[mp.t1$freq=="HF"]-mp.t1$MeanRT[mp.t1$freq=="LF"]), freq=F, xlab=" ", ylab="Density", main="Mean RT differences (in ms) between HF and LF words (by subjects)")
lines(density((mp.t1$MeanRT[mp.t1$freq=="HF"]-mp.t1$MeanRT[mp.t1$freq=="LF"])))
par(mfrow=c(1, 2))
hist(mp.t2$MeanRT[mp.t2$freq=="HF"], freq=F, xlab=" ", ylab="Density", main="Mean RTs to HF words (by items)")
lines(density(mp.t2$MeanRT[mp.t2$freq=="HF"]))
hist(mp.t2$MeanRT[mp.t2$freq=="LF"], freq=F, xlab=" ", ylab="Density", main="Mean RTs to LF words (by items)")
lines(density(mp.t2$MeanRT[mp.t1$freq=="LF"]))
par(mfrow=c(1, 1))
shapiro.test(mp.t1$MeanRT[mp.t1$freq=="HF"]-mp.t1$MeanRT[mp.t1$freq=="LF"])
shapiro.test(mp.t2$MeanRT[mp.t2$freq=="HF"])
shapiro.test(mp.t2$MeanRT[mp.t2$freq=="LF"])
var.test(mp.t2$MeanRT[mp.t2$freq=="HF"], mp.t2$MeanRT[mp.t2$freq=="LF"])
t1=t.test(mp.t1$MeanRT~mp.t1$freq, paired=T)
t1
WC1a=wilcox.test(mp.t1$MeanRT~mp.t1$freq, paired=T)
WC1a
WC1b=wilcox.test(mp.t1$MeanRT~mp.t1$freq, paired=T, correct=F)
WC1b
t2=t.test(mp.t2$MeanRT~mp.t2$freq, paired=F, var.equal=T)
t2

###Write-up: Response times were shorter for high-frequency words (M = 549 ms, SD = 87; median = 538 ms, IQR = 97) than for low-frequency words (M = 593 ms, SD = 104; median = 565 ms, IQR = 117). This difference was significant both by subjects (normality deviation: p < .05; Wilcoxon signed rank test: p < .001) and by items (t(94) = 6.63, p < .001).

###Write-up (longer version): Under the by-subjects analysis, a Shapiro-Wilk test indicated that the distribution of the mean response time (RT) differences between high-frequency (HF) and low-frequency (LF) words deviated significantly from normality: W = 0.93, p < 0.05. Therefore, a non-parametric Wilcoxon signed rank test was conducted on these data. The results showed that HF words (median = 538 ms, IQR = 97) were responded to faster than LF words (median = 565 ms, IQR = 117), V=0, p < .001. Under the by-items analysis, the distributions of the RTs for HF and LF words did not deviate significantly from normality (HF: W = 0.98, p = 0.49; LF: W = 0.96, p = 0.12). There was also no reliable difference in the variability of the RTs for these words, F(47, 47) = 0.73, p = 0.28. Therefore, a parametric independent samples t-test was conducted. This analysis also indicated significantly shorter RTs for HF words than for LF words, t(94) = 6.63, p < .001.

###Write-up (practical alternative): Response times were shorter for high-frequency words (M = 549 ms, SD = 87) than for low-frequency words (M = 593 ms, SD = 104), t1(31) = 9.61, p < .001; t2(94) = 6.63, p < .001.


mp_ER.t1 = aggregate(mp$error, list(mp$subj, mp$freq), mean)
mp_ER.t2 = aggregate(mp$error, list(mp$itemN, mp$freq), mean)
colnames(mp_ER.t1) = c("subj", "freq", "MeanER")
colnames(mp_ER.t2) = c("itemN", "freq", "MeanER")
mp_ER.t1$subj=as.factor(mp_ER.t1$subj)
mp_ER.t2$itemN=as.factor(mp_ER.t2$itemN)
mp_ER.t1.mean=tapply(mp_ER.t1$MeanER, mp_ER.t1$freq, mean)
mp_ER.t1.sd=tapply(mp_ER.t1$MeanER, mp_ER.t1$freq, sd)
mp_ER.t1.median=tapply(mp_ER.t1$MeanER, mp_ER.t1$freq, median)
mp_ER.t1.IQR=tapply(mp_ER.t1$MeanER, mp_ER.t1$freq, IQR)
mp_ER.t1.mean
mp_ER.t1.sd
mp_ER.t1.median
mp_ER.t1.IQR
boxplot(mp_ER.t1$MeanER[mp_ER.t1$freq=="HF"]-mp_ER.t1$MeanER[mp_ER.t1$freq=="LF"], notch=T, ylim=c(-.30, .30), ylab=" ", main="Mean ER difference between HF and LF words (by subjects)")
abline(h=0, lty=2,col="grey")
hist((mp_ER.t1$MeanER[mp.t1$freq=="HF"]-mp_ER.t1$MeanER[mp_ER.t1$freq=="LF"]), freq=F, xlab=" ", ylab="Density", main="Mean ER differences between HF and LF words (by subjects)")
lines(density((mp_ER.t1$MeanER[mp_ER.t1$freq=="HF"]-mp_ER.t1$MeanER[mp_ER.t1$freq=="LF"])))
par(mfrow=c(1, 2))
hist(mp_ER.t2$MeanER[mp_ER.t2$freq=="HF"], freq=F, xlab="ERs", ylab="Density", main="Mean ERs to HF words (by items)")
lines(density(mp_ER.t2$MeanER[mp_ER.t2$freq=="HF"]))
hist(mp_ER.t2$MeanER[mp_ER.t2$freq=="LF"], freq=F, xlab="ERs", ylab="Density", main="Mean ERs to LF words (by items)")
lines(density(mp_ER.t2$MeanER[mp.t1$freq=="LF"]))
par(mfrow=c(1, 1))
shapiro.test(mp_ER.t1$MeanER[mp_ER.t1$freq=="HF"]-mp_ER.t1$MeanER[mp_ER.t1$freq=="LF"])
shapiro.test(mp_ER.t2$MeanER[mp_ER.t2$freq=="HF"])
shapiro.test(mp_ER.t2$MeanER[mp_ER.t2$freq=="LF"])
t1=t.test(mp_ER.t1$MeanER~mp_ER.t1$freq, paired=T)
t1
WC1a=wilcox.test(mp_ER.t1$MeanER~mp_ER.t1$freq, paired=T)
WC1a
WC1b=wilcox.test(mp_ER.t1$MeanER~mp_ER.t1$freq, paired=T, correct=F)
WC1b
t2=t.test(mp_ER.t2$MeanER~mp_ER.t2$freq, paired=F, var.equal=T)
t2
WC1a=wilcox.test(mp_ER.t2$MeanER~mp_ER.t2$freq, paired=F)
WC1a
WC1b=wilcox.test(mp_ER.t2$MeanER~mp_ER.t2$freq, paired=F, correct=F)
WC1b

###Write-up: Error rates were lower for high-frequency words (M = 5.86%, SD = 5.81; median = 4.17%, IQR = 4.17) than for low-frequency words (M = 13.74%, SD = 7.90; median = 10.42%, IQR = 13.02). This difference was significant both by subjects (normality deviation: p < .01; Wilcoxon signed rank test: p < .001) and by items (normality deviations: p < .01; Wilcoxon rank sum test: p < .001).

###Write-up (longer version): Under the by-subjects analysis, a Shapiro-Wilk test indicated that the distribution of the mean error rate (ER) differences between high-frequency (HF) and low-frequency (LF) words deviated significantly from normality: W = 0.90, p < 0.01. Therefore, a non-parametric Wilcoxon signed rank test was conducted on these data. The results showed a lower ER for HF words (median = 4.17%, IQR = 4.17) than for LF words (median = 10.42%, IQR = 13.02), V=0, p < .001. Under the by-items analysis, the distributions of the ERs for HF and LF words also deviated significantly from normality (HF: W = 0.91, p < 0.01; LF: W = 0.89, p < 0.001). A Wilcoxon rank sum test again indicated that significantly lower ER for HF words than for LF words, W = 622.5, p < 0.001.

###Write-up (practical alternative): Error rates were lower for high-frequency words (M = 5.86%, SD = 5.81) than for low-frequency words (M = 13.74%, SD = 7.90), t1(31) = 7.05, p < .001; t(94) = 4.61, p < .001.
