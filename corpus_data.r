## PS6 ##

#(1) The Lextale is a very short, internet-based vocabulary/language proficiency test (http://www.lextale.com/). Using the 'tests.csv' dataset, assess the relationship between scores on this test ('Lextale') and the TOEFL test ('TOEFL_iBT_TS') for thirty highly-proficient non-native speakers of English. Also, examine the relationship between scores on the Lextale test ('Lextale') and each of the TOEFL subtests -- the reading subtest ('TOEFL_iBT_RS'), the writing subtest ('TOEFL_iBT_WS'), the listening subtest ('TOEFL_iBT_LS'), and the speaking subtest ('TOEFL_iBT_SS'). Plot each of these relationships and conduct a correlation test for each one. Without worrying too much about reporting the details of your statistical tests, briefly discuss your findings.

tests=read.csv("tests.csv", header=T)
plot(tests$Lextale~tests$TOEFL_iBT_TS)
cor.test(tests$Lextale, tests$TOEFL_iBT_TS)
plot(tests$Lextale~tests$TOEFL_iBT_RS)
cor.test(tests$Lextale, tests$TOEFL_iBT_RS)
plot(tests$Lextale~tests$TOEFL_iBT_WS)
cor.test(tests$Lextale, tests$TOEFL_iBT_WS)
plot(tests$Lextale~tests$TOEFL_iBT_LS)
cor.test(tests$Lextale, tests$TOEFL_iBT_LS)
plot(tests$Lextale~tests$TOEFL_iBT_SS)
cor.test(tests$Lextale, tests$TOEFL_iBT_SS)

####Write-up: In general, there was a positive correlation between the Lextale scores and the scores on the TOEFL test as well as on each of its subtests. This correlation was significant when the Lextale was compared with total TOEFL scores, the reading subtest, and the speaking subtest (p<.05). This correlation was not significant when the Lextale was compared with the writing subtest or the listening subtest.



#(2) In a corpus of academic texts (3,000,000 tokens), the logical connector 'moreover' occurs 324 times. In a smaller corpus of academic texts written by non-native speakers (500,000 tokens), this word occurs 88 times. Use the binomial test to determine whether the observed frequency of this word in the non-native speaker corpus differs significantly from the expected frequency based on the larger corpus of academic texts. Briefly discuss your findings.

binom.test(88, 500000, 324/3000000)

####Write-up: According to a binomial test, the observed frequency of the word 'moreover' in the non-native speaker corpus differs significantly from the expected frequency based on the larger corpus of academic texts (p<0.001). More specifically, the word 'moreover' occurs significantly more often in the non-native speaker corpus. It seems that non-native speakers are using this word at a much higher rate than is standard in academic writing.



#(3a) Evaluate the distribution of 'AnimacyOfRec' in the 'verbs' data set (in languageR).
	#generate descriptive statistics
	#create an illustrative graph
	#conduct a chi-squared goodness-of-fit test
	#briefly report your findings (along with your interpretation)

library(languageR)
AnimacyOfRec=table(verbs$AnimacyOfRec)
AnimacyOfRec
barplot(AnimacyOfRec, space=0, names.arg=c("Animate", "Inanimate"), xlab="Animacy of Recipient", ylab="Frequency", ylim=c(0,1000))
text(0.5, AnimacyOfRec[1]/2, paste(AnimacyOfRec[1]))
text(1.5, AnimacyOfRec[2]/2, paste(AnimacyOfRec[2]))
AnimacyOfRecX=chisq.test(AnimacyOfRec)
AnimacyOfRecX

####Write-up: According to a chi-squared goodness-of-fit test, animate recepients occur significantly more often than inanimate recepients (X^2=608.06; df=1, p<.001).



#(3b) Evaluate the distribution of 'AnimacyOfTheme' in the 'verbs' data set (in languageR).
	#generate descriptive statistics
	#create an illustrative graph
	#conduct a chi-squared goodness-of-fit test
	#briefly report your findings (along with your interpretation)
	
AnimacyOfTheme=table(verbs$AnimacyOfTheme)
AnimacyOfTheme
barplot(AnimacyOfTheme, space=0, names.arg=c("Animate", "Inanimate"), xlab="Animacy of Theme", ylab="Frequency", ylim=c(0,1000))
text(0.5, AnimacyOfTheme*5, paste(AnimacyOfTheme[1]))
text(1.5, AnimacyOfTheme[2]/2, paste(AnimacyOfTheme[2]))
AnimacyOfThemeX=chisq.test(AnimacyOfTheme)
AnimacyOfThemeX

####Write-up: According to a chi-squared goodness-of-fit test, inanimate themes occur significantly more often than animate themes (X^2=879.16; df=1, p<.001).



#(4) Use the 'RussianRC.csv' dataset to evaluate the relationship between relative-clause type (subject-extracted relative clause, or SRC; object-extracted relative clause, or ORC) and relative-clause word order (canonical, scrambled) in Russian.
	#generate descriptive statistics
	#create an illustrative graph
	#conduct a chi-squared test for independence
	#briefly report your findings (along with your interpretation)
		#as part of this report, for each RC type, determine whether there is a statistically reliable word-order preference (using a chi-squared goodness-of-fit test)

RussianRC=read.csv("RussianRC.csv", header=T)
RussianRC_table=table(RussianRC$RCtype, RussianRC$RCwordorder)
RussianRC_table
RussianRC_perc=prop.table(RussianRC_table, margin=1)
RussianRC_perc
plot(RussianRC$RCwordorder~RussianRC$RCtype, ylab="RC Word Order", xlab="RC Type")
chisq.test(RussianRC_table, correct=F)
chisq.test(RussianRC_table[1,], correct=F)
chisq.test(RussianRC_table[2,], correct=F)

####Write-up: According to a chi-squared test for independence, the scrambled word order is dispreferred in SRCs, but preferred in the ORCs (X^2 = 365.09; df = 1; p<0.001). Pair-wise chi-squared goodness-of-fit tests show that the canonical word order occurs more frequently than the scrambled order in SRCs (X^2 = 557.43, df = 1, p<.001), while the scrambled word order occurs more frequently than the canonical order in ORCs (X^2 = 7.31; df = 1; p<.01).