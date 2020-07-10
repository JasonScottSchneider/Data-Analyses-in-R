#(1) Determine whether the plant and animal words in the 'ratings' data set (in languageR) differ in terms of the variability of their mean familiarity ratings ('meanFamiliarity'). 
	# generate descriptive statistics and an illustrative graph
	# check whether the relevant distributions deviate from normality by generating illustrative histograms and by running the appropriate statistical tests
	# run the appropriate statistical test to check for a difference in variability
	# briefly report your findings

library(languageR)
tapply(ratings$meanFamiliarity, ratings$Class, mean)
tapply(ratings$meanFamiliarity, ratings$Class, sd)
boxplot(ratings$meanFamiliarity~ratings$Class, notch=T)
par(mfrow=c(1,2))
hist(ratings$meanFamiliarity[ratings$Class=="animal"], freq=F, xlab="Mean Familiarity", ylab="Density", main="Animal Words")
lines(density(ratings$meanFamiliarity[ratings$Class=="animal"]))
hist(ratings$meanFamiliarity[ratings$Class=="plant"], freq=F, xlab="Mean Familiarity", ylab="Density", main="Plant Words")
lines(density(ratings$meanFamiliarity[ratings$Class=="plant"]))
par(mfrow=c(1,1))
tapply(ratings$meanFamiliarity, ratings$Class, shapiro.test)
var.test(ratings$meanFamiliarity~ratings$Class)

###Write-up: There is no reliable difference in variability for the mean familiarity ratings of plant and animal words in the 'ratings' data set, F(45, 34) = 1.41, p = 0.30.



#(2) Evaluate whether the length of the /n/ in the Dutch prefix ont- ('DurationPrefixNasal') differs from a hypothesized mean/median of .053s in the 'durationsOnt' data set (in languageR).
	# generate descriptive statistics and an illustrative graph
	# run both parametric and non-parametric tests
		# (determine which is more appropriate for these data using the appropriate test)
	# briefly report your findings (only for the analysis you select; not for both)
	
library(languageR)
mean(durationsOnt$DurationPrefixNasal)
sd(durationsOnt$DurationPrefixNasal)
median(durationsOnt$DurationPrefixNasal)
IQR(durationsOnt$DurationPrefixNasal)
boxplot(durationsOnt$DurationPrefixNasal, ylab="Length", main="Length of /n/ in the Dutch Prefix ont-", notch=T); abline(h=.053, lty=2,col="grey")
shapiro.test(durationsOnt$DurationPrefixNasal)
t.test(durationsOnt$DurationPrefixNasal, mu = 0.053)
wilcox.test(durationsOnt$DurationPrefixNasal, mu = 0.053, correct=F)

###Write-up: The average length of /n/ in the prefix ont- was 0.050s (SD = 0.021). According to a one-sample t-test, this mean does not differ significantly from the hypothesized mean of 0.053s, t(101) = -1.50, p > .05.



#(3) Evaluate whether there is a reliable difference between the mean familiarity ratings ('meanFamiliarity') for the plant and animal words in the 'ratings' data set (in languageR).
	# based on your findings from (1), select the appropriate statistical test
	# briefly report your findings

t.test(ratings$meanFamiliarity~ratings$Class, paired=F, var.equal=T)

###Write-up: According to an independent samples t-test, the mean familiarity ratings for plant words (M = 4.37, SD = .85) were higher than for animal words (M = 3.51, SD = .85), t(79)=4.05, p<.001.



#(4) Evaluate whether the length of the theme ('LengthOfTheme') differs depending on the realization of the recipient ('RealizationOfRec') in the 'verbs' data set (in languageR).
	# generate descriptive statistics and an illustrative graph
	# run both parametric and non-parametric tests
		#(determine which is more appropriate for these data using the appropriate tests)
	# briefly report your findings (only for the analysis you select; not for all)

tapply(verbs$LengthOfTheme, verbs$RealizationOfRec, mean)
tapply(verbs$LengthOfTheme, verbs$RealizationOfRec, sd)
tapply(verbs$LengthOfTheme, verbs$RealizationOfRec, median)
tapply(verbs$LengthOfTheme, verbs$RealizationOfRec, IQR)
boxplot(verbs$LengthOfTheme~verbs$RealizationOfRec, notch=T, ylim=c(0, 4), ylab="Length", main="Length of Theme for NP and PP Recipients", col=c("orange", "blue"))
par(mfrow=c(1, 2))
hist(verbs$LengthOfTheme[verbs$RealizationOfRec=="NP"], freq=F, xlab="Length", ylab="Density", main=" ")
lines(density(verbs$LengthOfTheme[verbs$RealizationOfRec=="NP"]))
hist(verbs$LengthOfTheme[verbs$RealizationOfRec=="PP"], freq=F, xlab="Length", ylab="Density", main=" ")
lines(density(verbs$LengthOfTheme[verbs$RealizationOfRec=="PP"]))
par(mfrow=c(1, 1))
tapply(verbs$LengthOfTheme, verbs$RealizationOfRec, shapiro.test)
fligner.test(verbs$LengthOfTheme~verbs$RealizationOfRec)
t.test(verbs$LengthOfTheme~verbs$RealizationOfRec, paired=F, var.equal=T)
t.test(verbs$LengthOfTheme~verbs$RealizationOfRec, paired=F)
wilcox.test(verbs$LengthOfTheme~verbs$RealizationOfRec, paired=F, correct=F)

###Write-up: According to a U-Test, the median length of themes in sentences with NP recipients (1.79, IQR = 1.30) differs significantly from the median length of themes in sentences with PP recipients (1.10, IQR = .92), W = 139120, p < .001. Themes are longer when the recipient is realized as an NP.



#(BONUS) Evaluate whether the length of the /n/ in the Dutch prefix ont- ('DurationPrefixNasal') differs significantly from the length of the vowel ('DurationPrefixVowel') in the 'durationsOnt' data set (in languageR).
	# generate descriptive statistics and an illustrative graph
	# run both parametric and non-parametric tests
		#(determine which is more appropriate for these data using the appropriate test)
	# briefly report your findings (only for the analysis you select; not for both)

mean(durationsOnt$DurationPrefixVowel)
sd(durationsOnt$DurationPrefixVowel)
median(durationsOnt$DurationPrefixVowel)
IQR(durationsOnt$DurationPrefixVowel)
mean(durationsOnt$DurationPrefixNasal)
sd(durationsOnt$DurationPrefixNasal)
median(durationsOnt$DurationPrefixNasal)
IQR(durationsOnt$DurationPrefixNasal)
boxplot(durationsOnt$DurationPrefixVowel, durationsOnt$DurationPrefixNasal, notch=T, ylab="Length", ylim=c(0, .15), main="Length of /o/ and /n/ in the Dutch Prefix ont-", names=c("/o/", "/n/"))
boxplot(durationsOnt$DurationPrefixVowel-durationsOnt$DurationPrefixNasal, notch=T, ylab="Differences in Length: Vowel-Nasal", ylim=c(-.1, .1)); abline(h=0, lty=2,col="grey")
shapiro.test(durationsOnt$DurationPrefixVowel-durationsOnt$DurationPrefixNasal)
t.test(durationsOnt$DurationPrefixVowel, durationsOnt$DurationPrefixNasal, paired=T)
wilcox.test(durationsOnt$DurationPrefixVowel, durationsOnt$DurationPrefixNasal, paired=T, correct=F)

###Write-up: In the Dutch prefix ont-, the vowel (M = 0.063s, SD = 0.021) is significantly longer than the nasal (M = 0.050s, SD = 0.021), t(101) = 4.59, p < .001. 