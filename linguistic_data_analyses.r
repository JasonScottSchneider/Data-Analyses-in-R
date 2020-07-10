#(1) 'RCs.csv'

#You are interested in how often relative clauses (RCs) modify the subject of a sentence (subject-modifying) and how often they modify the object of the sentence (object-modifying). You are also interested in the relative frequency of subject- and object-extracted RCs, and whether that frequency differs as a function of modification position. With these questions in mind, you have examined a small corpus to identify sentences like the following:

	#subject-modifying / subject-extracted  
		#The soldier who roughly pushed the sailor broke a bottle on the bar.
	
	#subject-modifying / object-extracted   
		#The soldier who the sailor roughly pushed broke a bottle on the bar.
	
	#object-modifying / subject-extracted	
		#The police arrested the soldier who roughly pushed the sailor.
	
	#object-modifying / object-extracted   	
		#The police arrested the soldier who the sailor roughly pushed.

#A dataset with sentences coded for these two factors is provided in 'RCs.csv'.

#(i) Examine whether object-modifying RCs occur more frequently than subject-modifying RCs.
#(ii) Examine whether subject-extracted RCs occur more frequently than object-extracted RCs.
#(iii) Evaluate whether the distribution of subject- and object-extracted RCs differs depending on the part of the sentence they modify.

#For each of these analyses, provide the relevant descriptive statistics, create an illustrative graph, run the appropriate statistical test(s), and provide a very brief write-up of your findings.

########################

RCs=read.csv("RCs.csv", header=T)

#(i)

RCs_mod_table=table(RCs$modified_NP)
RCs_mod_table
RCs_mod_perc=prop.table(RCs_mod_table)
RCs_mod_perc
barplot(RCs_mod_table, space=0, names.arg=c("object-modifying", "subject-modifying"), ylab="Frequency", xlab="RC Position", ylim=c(0,400))
text(0.5, RCs_mod_table[1]/2, paste(RCs_mod_table[1]))
text(1.5, RCs_mod_table[2]/2, paste(RCs_mod_table[2]))
chisq.test(RCs_mod_table)

#According to a chi-square goodness-of-fit test, the distribution of subject- and object-modifying RCs differs significantly from the expected distribution; x^2=159.43, df=1, p<.001. The frequency of RCs that modify subjects (82) is reliably lower than the frequency of relative clauses that modify objects (342).


#(ii)

RCs_extract_table=table(RCs$extraction_type)
RCs_extract_table
RCs_extract_perc=prop.table(RCs_extract_table)
RCs_extract_perc
barplot(RCs_extract_table, space=0, names.arg=c("object-extracted", "subject-extracted"), ylab="Frequency", xlab="RC Type", ylim=c(0,400))
text(0.5, RCs_extract_table[1]/2, paste(RCs_extract_table[1]))
text(1.5, RCs_extract_table[2]/2, paste(RCs_extract_table[2]))
chisq.test(RCs_extract_table)

#According to a chi-square goodness-of-fit test, the distribution of subject- and object-extracted relative clauses differs significantly from the expected distribution; x^2=51.66, df=1, p<.001. Subject-extracted RCs (286) occur significantly more often than object-extracted RCs (138).


#(iii)

RCs_table=table(RCs$modified_NP, RCs$extraction_type)
RCs_table
RCs_perc=prop.table(RCs_table, margin=1)
RCs_perc
plot(RCs$extraction_type~RCs$modified_NP, ylab="RC Type", xlab="RC Position")
chisq.test(RCs_table, correct=F)
###optional additional analyses:
chisq.test(RCs_table[1,], correct=F)
chisq.test(RCs_table[2,], correct=F)

#The distribution of subject- and object-extracted relative clauses does not differ depending on the part of the sentence they modify (x^2=.76, df=1, p=.38). Subject-extracted relative clauses occur more often than object-extracted relative clauses regardless of whether these clauses modify the sentential subject (x^2=5.90, df=1, p=.02) or object (x^2=46.42, df=1, p<.001).

########################



#(2) 'edetect.csv'

#You run an experiment in which you present words like 'press' and 'chair' *very briefly* (for 66 ms). The subject's task is simply to identify whether or not the word contained the letter 'e'. Half of the words contained an 'e'. Evaluate whether participants were better than chance (hypothesized mean = .5) at identifying whether or not the stimuli contained an 'e' using the 'edetect' data set (the raw data for this experiment). (That is, evaluate whether participants' mean error rates were reliably lower than .50. For this analysis, you only need to analyze by-subjects means.)

#The answer to this question should include 
	#(i) descriptive statistics on the mean rate of incorrect identification (i.e., mean error rate; note that '1' in the 'error' column means the item was answered incorrectly);
	#(ii) the complete set of code that you used to solve this problem;
	#(iii) a very short write-up of the results of your analyses.

########################

edetect=read.csv("edetect.csv", header=TRUE)
edetect.means=aggregate(edetect$error, list(edetect$subj), mean)
colnames(edetect.means) = c("subj", "ER")
meanER = mean(edetect.means$ER)
sdER = sd(edetect.means$ER)
meanER
sdER
shapiro.test(edetect.means$ER)
t.test(edetect.means$ER, mu=.5)

# According to a one-sample t-test, participants detected the letter 'e' at a rate that was significantly better than chance (mean error rate: .42 (SD=.08); hypothesized mean: .50; t(16)=4.48, p<.001).

########################



#(3) 'reading_strategies.csv'

#You have gathered data from second-language readers of English to examine whether readers at different proficiency levels differ in terms of their use of six reading strategies -- three cognitive strategies (Comprehending, Memory, Retrieval) and three metacognitive strategies (Planning, Monitoring, Evaluation). [See Kim, 2016 for definitions of these strategies.] You have classified these readers into three proficiency levels -- high reading proficiency (HRP), mid reading proficiency (MRP), and low reading proficiency (LRP). The use of each strategy is represented by a value ranging from 0 (they never use it) to 5 (they often use it).

#Analyze the data for each of the reading strategies. 
	#(i) Generate descriptive statistics and an illustrative graph.
  #(ii) Use the appropriate statistical test to examine whether there is a significant difference among the reading proficiency groups in termns of their use of this strategy.
  #(iii) If there is a difference among the groups, use the apporiate statistical test(s) to determine the nature of this difference.
	
########################
	
reading_strategies=read.csv("reading_strategies.csv", header=T)
reading_strategies$Reading_Prof=factor(reading_strategies$Reading_Prof, levels=c("HRP","MRP","LRP"))

tapply(reading_strategies$Comprehending, reading_strategies$Reading_Prof, mean)
tapply(reading_strategies$Comprehending, reading_strategies$Reading_Prof, sd)
boxplot(reading_strategies$Comprehending~reading_strategies$Reading_Prof, names=c("HRP", "MRP", "LRP"), col=c("lightblue", "blue", "orange"), notch=T);
text(1:3, tapply(reading_strategies$Comprehending, reading_strategies$Reading_Prof, mean), "X")
title(main="Comprehending Strategy Use for\n Different Reading Proficiency Groups", ylab = "Mean Strategy Use")

tapply(reading_strategies$Comprehending, reading_strategies$Reading_Prof, shapiro.test)
bartlett.test(reading_strategies$Comprehending~reading_strategies$Reading_Prof)
shapiro.test(residuals(lm(reading_strategies$Comprehending~reading_strategies$Reading_Prof)))

oneway.test(reading_strategies$Comprehending~reading_strategies$Reading_Prof, var.equal=F)

var.test(reading_strategies$Comprehending[reading_strategies$Reading_Prof=="HRP"], reading_strategies$Comprehending[reading_strategies$Reading_Prof=="MRP"])
var.test(reading_strategies$Comprehending[reading_strategies$Reading_Prof=="HRP"], reading_strategies$Comprehending[reading_strategies$Reading_Prof=="LRP"])
var.test(reading_strategies$Comprehending[reading_strategies$Reading_Prof=="MRP"], reading_strategies$Comprehending[reading_strategies$Reading_Prof=="LRP"])

t.test(reading_strategies$Comprehending[reading_strategies$Reading_Prof=="HRP"], reading_strategies$Comprehending[reading_strategies$Reading_Prof=="MRP"], var.equal=TRUE)
t.test(reading_strategies$Comprehending[reading_strategies$Reading_Prof=="HRP"], reading_strategies$Comprehending[reading_strategies$Reading_Prof=="LRP"])
t.test(reading_strategies$Comprehending[reading_strategies$Reading_Prof=="MRP"], reading_strategies$Comprehending[reading_strategies$Reading_Prof=="LRP"])

#Write-up: A Welch-corrected one-way ANOVA indicated a significant difference among the three reading groups in terms of their comprehending strategy use (F(2, 90.43)=4.60, p<.05). Follow-up analyses (Bonferroni corrected alpha = .017) revealed that both the HRP and MRP groups used this strategy more often than the LRP group (HRP vs. LRP: Welch two sample t-test, t(68.57)=3.01, p=.004; MRP vs. LRP: Welch two sample t-test, t(76.58)=2.52, p=.014). 


tapply(reading_strategies$Memory, reading_strategies$Reading_Prof, mean)
tapply(reading_strategies$Memory, reading_strategies$Reading_Prof, sd)
boxplot(reading_strategies$Memory~reading_strategies$Reading_Prof, names=c("HRP", "MRP", "LRP"), col=c("lightblue", "blue", "orange"), notch=T);
text(1:3, tapply(reading_strategies$Memory, reading_strategies$Reading_Prof, mean), "X")
title(main="Memory Strategy Use for\n Different Reading Proficiency Groups", ylab = "Mean Strategy Use")
tapply(reading_strategies$Memory, reading_strategies$Reading_Prof, shapiro.test)
bartlett.test(reading_strategies$Memory~reading_strategies$Reading_Prof)
shapiro.test(residuals(lm(reading_strategies$Memory~reading_strategies$Reading_Prof)))
summary(aov(reading_strategies$Memory~reading_strategies$Reading_Prof))

#Write-up: A one-way ANOVA indicated no significant difference among the three reading groups in terms of their memory strategy use (F(2, 150)=2.16, p=.12).


tapply(reading_strategies$Retrieval, reading_strategies$Reading_Prof, mean)
tapply(reading_strategies$Retrieval, reading_strategies$Reading_Prof, sd)
boxplot(reading_strategies$Retrieval~reading_strategies$Reading_Prof, names=c("HRP", "MRP", "LRP"), col=c("lightblue", "blue", "orange"), notch=T);
text(1:3, tapply(reading_strategies$Retrieval, reading_strategies$Reading_Prof, mean), "X")
title(main="Retrieval Strategy Use for\n Different Reading Proficiency Groups", ylab = "Mean Strategy Use")
tapply(reading_strategies$Retrieval, reading_strategies$Reading_Prof, shapiro.test)
tapply(reading_strategies$Retrieval, reading_strategies$Reading_Prof, median)
tapply(reading_strategies$Retrieval, reading_strategies$Reading_Prof, IQR)
kruskal.test(reading_strategies$Retrieval~reading_strategies$Reading_Prof)

#Write-up: A Kruskal-Wallis rank sum test (conducted due to a significant deviation from the normal distribution for the LRP group, Shapiro-Wilk normality test: p=.04) indicated no significant difference among the three reading groups in terms of their retrieval strategy use (p=.52).


tapply(reading_strategies$Planning, reading_strategies$Reading_Prof, mean)
tapply(reading_strategies$Planning, reading_strategies$Reading_Prof, sd)
boxplot(reading_strategies$Planning~reading_strategies$Reading_Prof, names=c("HRP", "MRP", "LRP"), col=c("lightblue", "blue", "orange"), notch=T);
text(1:3, tapply(reading_strategies$Planning, reading_strategies$Reading_Prof, mean), "X")
title(main="Planning Strategy Use for\n Different Reading Proficiency Groups", ylab = "Mean Strategy Use")
tapply(reading_strategies$Planning, reading_strategies$Reading_Prof, shapiro.test)
bartlett.test(reading_strategies$Planning~reading_strategies$Reading_Prof)
shapiro.test(residuals(lm(reading_strategies$Planning~reading_strategies$Reading_Prof)))
summary(aov(reading_strategies$Planning~reading_strategies$Reading_Prof))
TukeyHSD(aov(reading_strategies$Planning~reading_strategies$Reading_Prof))
t.test(reading_strategies$Planning[reading_strategies$Reading_Prof=="HRP"], reading_strategies$Planning[reading_strategies$Reading_Prof=="MRP"], var.equal=TRUE)
t.test(reading_strategies$Planning[reading_strategies$Reading_Prof=="HRP"], reading_strategies$Planning[reading_strategies$Reading_Prof=="LRP"], var.equal=TRUE)
t.test(reading_strategies$Planning[reading_strategies$Reading_Prof=="MRP"], reading_strategies$Planning[reading_strategies$Reading_Prof=="LRP"], var.equal=TRUE)

#Write-up: A one-way ANOVA indicated a significant difference among the three reading groups in terms of their planning strategy use (F(2, 150)=3.78, p<.05). Follow-up analyses (Bonferroni corrected alpha = .017) revealed that the HRP group used this strategy more often than the LRP group (t(99)=2.58, p=.011). 


tapply(reading_strategies$Monitoring, reading_strategies$Reading_Prof, mean)
tapply(reading_strategies$Monitoring, reading_strategies$Reading_Prof, sd)
boxplot(reading_strategies$Monitoring~reading_strategies$Reading_Prof, names=c("HRP", "MRP", "LRP"), col=c("lightblue", "blue", "orange"), notch=T);
text(1:3, tapply(reading_strategies$Monitoring, reading_strategies$Reading_Prof, mean), "X")
title(main="Monitoring Strategy Use for\n Different Reading Proficiency Groups", ylab = "Mean Strategy Use")
tapply(reading_strategies$Monitoring, reading_strategies$Reading_Prof, shapiro.test)
bartlett.test(reading_strategies$Monitoring~reading_strategies$Reading_Prof)
shapiro.test(residuals(lm(reading_strategies$Monitoring~reading_strategies$Reading_Prof)))
summary(aov(reading_strategies$Monitoring~reading_strategies$Reading_Prof))

#Write-up: A one-way ANOVA indicated no significant difference among the three reading groups in terms of their monitoring strategy use (F<1).


tapply(reading_strategies$Evaluation, reading_strategies$Reading_Prof, mean)
tapply(reading_strategies$Evaluation, reading_strategies$Reading_Prof, sd)
boxplot(reading_strategies$Evaluation~reading_strategies$Reading_Prof, names=c("HRP", "MRP", "LRP"), col=c("lightblue", "blue", "orange"), notch=T);
text(1:3, tapply(reading_strategies$Evaluation, reading_strategies$Reading_Prof, mean), "X")
title(main="Evaluation Strategy Use for\n Different Reading Proficiency Groups", ylab = "Mean Strategy Use")
tapply(reading_strategies$Evaluation, reading_strategies$Reading_Prof, shapiro.test)
tapply(reading_strategies$Evaluation, reading_strategies$Reading_Prof, median)
tapply(reading_strategies$Evaluation, reading_strategies$Reading_Prof, IQR)
kruskal.test(reading_strategies$Evaluation~reading_strategies$Reading_Prof)
var.test(reading_strategies$Evaluation[reading_strategies$Reading_Prof=="HRP"], reading_strategies$Evaluation[reading_strategies$Reading_Prof=="MRP"])
t.test(reading_strategies$Evaluation[reading_strategies$Reading_Prof=="HRP"], reading_strategies$Evaluation[reading_strategies$Reading_Prof=="MRP"], var.equal=TRUE)
wilcox.test(reading_strategies$Evaluation[reading_strategies$Reading_Prof=="HRP"], reading_strategies$Evaluation[reading_strategies$Reading_Prof=="LRP"], correct=F)
wilcox.test(reading_strategies$Evaluation[reading_strategies$Reading_Prof=="MRP"], reading_strategies$Evaluation[reading_strategies$Reading_Prof=="LRP"], correct=F)

#Write-up: A Kruskal-Wallis rank sum test (conducted due to a significant deviation from the normal distribution for the LRP group, Shapiro-Wilk normality test: p=.02) indicated a significant difference among the three reading groups in terms of their evaluation strategy use (Kruskal-Wallis chi-squared = 6.59, df = 2, p < 0.05). Follow-up analyses (Bonferroni corrected alpha = .017) revealed that the HRP group used this strategy marginally significantly more often than the MRP group (t(107)=2.37, p=.020).

#[A complete write-up of these analyses should include means, sds, medians, and IQRs.]

########################



#(4) 'RC_attach.csv'

#You are interested in RC attachment preferences in English sentences like the following:

#The son of the actress who was on the balcony was under investigation.

#Many studies have shown that English native speakers prefer to interpret this sentence to mean that the actress (not her son) was on the balcony (a low attachment preference). However, self-paced reading studies have shown conflicting results on sentences like the following:

#(a) (low) The son of the actress who shot herself was under investigation.
#(b) (high) The son of the actress who shot himself was under investigation.

#Some studies have shown that the reflexive is read more slowly in sentences like (b) -- consistent with a low attachment preference. Other studies have shown no reading time differences. You think that this may be due to task insensitivity. So you run an experiment in which two groups of subjects read sentences like these. One group ('window') reads these sentences under the traditional self-paced, moving-window reading paradigm. The other group ('maze') reads these sentences in a reading task that requires incremental integration of words into the sentence.

#The 'RC_attach.csv' dataset has the reading times at the reflexive in both sentence types (low, high) for both groups of subjects. Evaluate whether these tasks are differentially sensitive to the processing difficulty that should occur at the reflexive in high attachment sentences.

  #(i) Generate data frames will allow you to do by-subjects and by-items analyses.
  #(ii) Report the relevant by-subjects descriptive statistics.
  #(iii) Generate an illustrative graph. (In this case, an interaction plot would be a good idea.)
  #(iV) Evaluate the hypothesis using the appropriate statistical tests -- i.e., a set of analyses that looks at the interaction of task type (maze, window) and attachment site (high, low) (be careful about repeated vs. non-repeated measures!) and that checks the effect of attachment site (high, low) under each task.
  #(v) Provide a short write-up of your results.

########################

RC_attach=read.csv("RC_attach.csv", header=TRUE)  

RC_attach.F1 = aggregate(RC_attach$rt, list(RC_attach$subj, RC_attach$task, RC_attach$attach_site), mean)
colnames(RC_attach.F1) = c("subj", "task", "attach_site", "MeanRT")
RC_attach.F2 = aggregate(RC_attach$rt, list(RC_attach$itemN, RC_attach$task, RC_attach$attach_site), mean)
colnames(RC_attach.F2) = c("itemN", "task", "attach_site", "MeanRT")
RC_attach.F1$subj=factor(RC_attach.F1$subj)
RC_attach.F2$itemN=factor(RC_attach.F2$itemN)

F1.mean=tapply(RC_attach.F1$MeanRT, list(RC_attach.F1$task, RC_attach.F1$attach_site), mean)
F1.sd=tapply(RC_attach.F1$MeanRT, list(RC_attach.F1$task, RC_attach.F1$attach_site), sd)
F1.mean
F1.sd

interaction.plot(RC_attach.F1$attach_site, RC_attach.F1$task, RC_attach.F1$MeanRT, col=c("blue","red"), xlab = "Attachment Site", ylab="Mean RT", trace.label="Task"); grid();

summary(aov(MeanRT~attach_site*task+Error(subj/attach_site), data=RC_attach.F1))
summary(aov(MeanRT~attach_site*task+Error(itemN/(attach_site*task)), data=RC_attach.F2))

window.F1=RC_attach.F1[RC_attach.F1$task=="window", ]
shapiro.test(window.F1$MeanRT[window.F1$attach_site=="high"]-window.F1$MeanRT[window.F1$attach_site=="low"])
window.F2=RC_attach.F2[RC_attach.F2$task=="window", ]
shapiro.test(window.F2$MeanRT[window.F2$attach_site=="high"]-window.F2$MeanRT[window.F2$attach_site=="low"])

maze.F1=RC_attach.F1[RC_attach.F1$task=="maze", ]
shapiro.test(maze.F1$MeanRT[maze.F1$attach_site=="high"]-maze.F1$MeanRT[maze.F1$attach_site=="low"])
maze.F2=RC_attach.F2[RC_attach.F2$task=="maze", ]
shapiro.test(maze.F2$MeanRT[maze.F2$attach_site=="high"]-maze.F2$MeanRT[maze.F2$attach_site=="low"])

summary(aov(MeanRT~attach_site+Error(subj/attach_site), data=window.F1))
wilcox.test(window.F1$MeanRT[window.F1$attach_site=="high"], window.F1$MeanRT[window.F1$attach_site=="low"], paired=T)
summary(aov(MeanRT~attach_site+Error(itemN/attach_site), data=window.F2))

summary(aov(MeanRT~attach_site+Error(subj/attach_site), data=maze.F1))
summary(aov(MeanRT~attach_site+Error(itemN/attach_site), data=maze.F2))

#Write-up: ANOVAs were conducted by subjects and items, with mean RT as the dependent variable and attachment type (high, low) and task type (window, maze) as predictor variables. The main effect of task type was significant (F1(1,62)=69.64, p<.001, F2(1,23)=282.6, p<.001), indicating that RTs were generally longer in the maze task than in the window task. There was also a significant main effect of attachment type (F1(1,62)=13.68, p<.001, F2(1,23)=17.77, p<.001), indicating that high attachment sentences took longer to process than low attachment sentences. This latter effect, however, was qualified by a significant interaction of attachment type and task type (F1(1,62)=5.85, p<.05, F2 (1,23)=5.08, p<.05). Indeed, while low attachment sentences had reliably faster RTs than high attachment sentences under the maze task (F1(1,31)=14.43, p<.001, F2(1,23)=20.28, p<.001), this difference was not statistically reliable under the window task (F1(1.31)=1.17, p=0.29, F2<1). [Note that these simple effects/pairwise comparisons could have been done using t-tests or the Wilcoxon signed rank test. Note also that a repeated-measures ANOVA/F-test (like the one I reported above) or a paired t-test is technically not justified for the by-subjects analysis of low vs. high under the window task. The Shapiro-Wilk test indicated that the paired differences here were not normally distributed. But it doesn't really matter -- the pairwise difference is not significant regardless of whether you use a parametric t-test/F-test or a non-parametric Wilcoxon signed rank test. Also, in cases like this -- where the focus is primarily/exclusively on repeated measures -- researchers often do not check distributional assumptions and just go with parametric statistical tests.] These results show that the window task is not as sensitive as the maze task to processing difficulty at the reflexive in the high attachment sentences. 

########################	
	
	

#(5) 'freq_prime_dur.csv'

#Your previous research has indicated that comparable levels of masked repetition priming (chair-CHAIR) are obtained for high-frequency (HF) and low-frequency (LF) words. These results were obtained with 66ms masked primes. You want to determine whether this is the case with 33ms primes as well. You run a masked repetition priming experiment with HF/LF words and two prime durations -- 33ms and 66ms. Evaluate whether the magnitude of priming for HF and LF words differs depending on prime duration. As always, only include items that were answered correctly (i.e., that have a '0' in the error column.) [Note that this analysis involves 3 factors -- frequency, prime type, and prime duration. A good place to start would be to conduct the 3-factor ANOVA by subjects and by items. Make sure to correctly identify your repeated and non-repeated measures in these analyses. You might then subset the data by prime duration (33ms, 66ms) and look at the effects of frequency, prime type, and their interaction at each duration.]

#Provide the relevant descriptive statistics, illustrative graphs (two interaction plots: one for the effects of frequency and prime type at 33ms, and another for the effects of frequency and prime type at 66ms), and a write-up of your statistical analyses. [Note that you probably will not have to analyze the data at the level of pairwise comparisons. The results should be clear after running the main 2x2x2 ANOVAs and the 2x2 ANOVAs at each prime duration.]

########################

freq_prime_dur=read.csv("freq_prime_dur.csv", header=T)
freq_prime_dur_cor=freq_prime_dur[freq_prime_dur$error == "0", ]
freq_prime_dur.F1 = aggregate(freq_prime_dur_cor$rt, list(freq_prime_dur_cor$subj, freq_prime_dur_cor$freq, freq_prime_dur_cor$ptype, freq_prime_dur_cor$dur), mean)
freq_prime_dur.F2 = aggregate(freq_prime_dur_cor$rt, list(freq_prime_dur_cor$itemN, freq_prime_dur_cor$freq, freq_prime_dur_cor$ptype, freq_prime_dur_cor$dur), mean)
colnames(freq_prime_dur.F1) = c("subj", "freq", "ptype", "dur", "Mean_RT")
colnames(freq_prime_dur.F2) = c("item", "freq", "ptype", "dur", "Mean_RT")
freq_prime_dur.F1$subj=as.factor(freq_prime_dur.F1$subj)
freq_prime_dur.F2$item=as.factor(freq_prime_dur.F2$item)
freq_prime_dur.F1_mean=tapply(freq_prime_dur.F1$Mean_RT, list(freq_prime_dur.F1$freq, freq_prime_dur.F1$ptype, freq_prime_dur.F1$dur), mean)
freq_prime_dur.F1_sd=tapply(freq_prime_dur.F1$Mean_RT, list(freq_prime_dur.F1$freq, freq_prime_dur.F1$ptype, freq_prime_dur.F1$dur), sd)
freq_prime_dur.F1_mean
freq_prime_dur.F1_sd

summary(aov(Mean_RT~freq*dur*ptype+Error(subj/(freq*dur*ptype)), data=freq_prime_dur.F1))
summary(aov(Mean_RT~freq*dur*ptype+Error(item/(dur*ptype)), data=freq_prime_dur.F2))

freq_prime_dur_33.F1=freq_prime_dur.F1[freq_prime_dur.F1$dur == "33ms", ]
freq_prime_dur_66.F1=freq_prime_dur.F1[freq_prime_dur.F1$dur == "66ms", ]
freq_prime_dur_33.F2=freq_prime_dur.F2[freq_prime_dur.F2$dur == "33ms", ]
freq_prime_dur_66.F2=freq_prime_dur.F2[freq_prime_dur.F2$dur == "66ms", ]

par(mfrow=c(2,1))
interaction.plot(freq_prime_dur_33.F1$ptype, freq_prime_dur_33.F1$freq, freq_prime_dur_33.F1$Mean_RT, xlab="Priming Condition", ylab="Mean RT (in ms)", trace.label="Word\nFrequency", main="Mean RT (in ms) for high- and low-frequency words\nin related and unrelated conditions at the 33ms prime duration", ylim=c(500,650))
interaction.plot(freq_prime_dur_66.F1$ptype, freq_prime_dur_66.F1$freq, freq_prime_dur_66.F1$Mean_RT, xlab="Priming Condition", ylab="Mean RT (in ms)", trace.label="Word\nFrequency", main="Mean RT (in ms) for high- and low-frequency words\nin related and unrelated conditions at the 66ms prime duration", ylim=c(500,650))
par(mfrow=c(1,1))

summary(aov(Mean_RT~freq*ptype+Error(subj/(freq*ptype)), data=freq_prime_dur_33.F1))
summary(aov(Mean_RT~freq*ptype+Error(item/(ptype)), data=freq_prime_dur_33.F2))
summary(aov(Mean_RT~freq*ptype+Error(subj/(freq*ptype)), data=freq_prime_dur_66.F1))
summary(aov(Mean_RT~freq*ptype+Error(item/(ptype)), data=freq_prime_dur_66.F2))

###These tests of simple effects might not be necessary###

freq_prime_dur_33_HF.F1=freq_prime_dur_33.F1[freq_prime_dur_33.F1$freq == "HF", ]
freq_prime_dur_33_LF.F1=freq_prime_dur_33.F1[freq_prime_dur_33.F1$freq == "LF", ]
freq_prime_dur_33_HF.F2=freq_prime_dur_33.F2[freq_prime_dur_33.F2$freq == "HF", ]
freq_prime_dur_33_LF.F2=freq_prime_dur_33.F2[freq_prime_dur_33.F2$freq == "LF", ]

shapiro.test(freq_prime_dur_33_HF.F1$Mean_RT[freq_prime_dur_33_HF.F1$ptype=="rel"]-freq_prime_dur_33_HF.F1$Mean_RT[freq_prime_dur_33_HF.F1$ptype=="unrel"])
shapiro.test(freq_prime_dur_33_HF.F2$Mean_RT[freq_prime_dur_33_HF.F2$ptype=="rel"]-freq_prime_dur_33_HF.F2$Mean_RT[freq_prime_dur_33_HF.F2$ptype=="unrel"])
shapiro.test(freq_prime_dur_33_LF.F1$Mean_RT[freq_prime_dur_33_LF.F1$ptype=="rel"]-freq_prime_dur_33_LF.F1$Mean_RT[freq_prime_dur_33_LF.F1$ptype=="unrel"])
shapiro.test(freq_prime_dur_33_LF.F2$Mean_RT[freq_prime_dur_33_LF.F2$ptype=="rel"]-freq_prime_dur_33_LF.F2$Mean_RT[freq_prime_dur_33_LF.F2$ptype=="unrel"])

summary(aov(Mean_RT~ptype+Error(subj/(ptype)), data=freq_prime_dur_33_HF.F1))
wilcox.test(freq_prime_dur_33_HF.F1$Mean_RT~freq_prime_dur_33_HF.F1$ptype, paired=T)
summary(aov(Mean_RT~ptype+Error(item/(ptype)), data=freq_prime_dur_33_HF.F2))

summary(aov(Mean_RT~ptype+Error(subj/(ptype)), data=freq_prime_dur_33_LF.F1))
wilcox.test(freq_prime_dur_33_LF.F1$Mean_RT~freq_prime_dur_33_LF.F1$ptype, paired=T)
summary(aov(Mean_RT~ptype+Error(item/(ptype)), data=freq_prime_dur_33_LF.F2))

freq_prime_dur_66_HF.F1=freq_prime_dur_66.F1[freq_prime_dur_66.F1$freq == "HF", ]
freq_prime_dur_66_LF.F1=freq_prime_dur_66.F1[freq_prime_dur_66.F1$freq == "LF", ]
freq_prime_dur_66_HF.F2=freq_prime_dur_66.F2[freq_prime_dur_66.F2$freq == "HF", ]
freq_prime_dur_66_LF.F2=freq_prime_dur_66.F2[freq_prime_dur_66.F2$freq == "LF", ]

shapiro.test(freq_prime_dur_66_HF.F1$Mean_RT[freq_prime_dur_66_HF.F1$ptype=="rel"]-freq_prime_dur_66_HF.F1$Mean_RT[freq_prime_dur_66_HF.F1$ptype=="unrel"])
shapiro.test(freq_prime_dur_66_HF.F2$Mean_RT[freq_prime_dur_66_HF.F2$ptype=="rel"]-freq_prime_dur_66_HF.F2$Mean_RT[freq_prime_dur_66_HF.F2$ptype=="unrel"])
shapiro.test(freq_prime_dur_66_LF.F1$Mean_RT[freq_prime_dur_66_LF.F1$ptype=="rel"]-freq_prime_dur_66_LF.F1$Mean_RT[freq_prime_dur_66_LF.F1$ptype=="unrel"])
shapiro.test(freq_prime_dur_66_LF.F2$Mean_RT[freq_prime_dur_66_LF.F2$ptype=="rel"]-freq_prime_dur_66_LF.F2$Mean_RT[freq_prime_dur_66_LF.F2$ptype=="unrel"])

summary(aov(Mean_RT~ptype+Error(subj/(ptype)), data=freq_prime_dur_66_HF.F1))
summary(aov(Mean_RT~ptype+Error(item/(ptype)), data=freq_prime_dur_66_HF.F2))
summary(aov(Mean_RT~ptype+Error(subj/(ptype)), data=freq_prime_dur_66_LF.F1))
summary(aov(Mean_RT~ptype+Error(item/(ptype)), data=freq_prime_dur_66_LF.F2))

#ANOVAs were conducted by subjects and items, with mean RT as the dependent variable and frequency (high, low), prime type (related, unrelated), and prime duration (66ms, 33ms) as predictor variables. There was a significant main effect of frequency (F1(1,31)=95.44, p<.001, F2(1,94)=45.38, p<.001), with high-frequency words responded to faster than low-frequency words. The main effect of prime type was also significant (F1(1,31)=86.09, p<.001, F2(1,94)=59.83, p<.001), indicating priming for both high- and low-frequency words at both prime durations. The main effect of prime duration was also significant (F1(1,31)=14.56, p<.001, F2(1,94)=11.21, p<.01), and there was a statistically significant interaction of prime type and duration (F1(1,31)=9.8, p<.01, F2(1,94)=7.823, p<0.01), indicating generally greater priming at the 66ms prime duration. The two-way frequency x prime type and frequency x duration interactions as well as the three-way frequency x prime type x duration were not statistically reliable. At the 66 ms prime duration, the main effects frequency (F1(1,31)=57.68, p<.001, F2(1,94)=29.42, p<.001) and prime type (F1(1,31)=94.32, p<.001, F2(1,94)=57.37, p<.001) were significant. At the 33 ms duration, there were also significant main effects of frequency (F1(1,31)=49.72, p<.001, F2(1,94)=29.83, p<.001) and prime type (F1(1,31)=12.21, p<.01, F2(1,94)=14.83, p<.001). The interaction of frequency and prime type was not significant at either prime duration (all F's < 1). This pattern of results clearly shows that magnitude of priming for HF and LF words does not differ depending on prime duration.

########################



#(6) 'brain.csv'

#You have measured changes in voltage on the scalp (using an EEG setup) when people read sentences involving pronouns. The sentences that you are particularly interested in are as follows:

#(a) When he saw the UFO, John told Mary to call the police.
#(b) When she saw the UFO, John told Mary to call the police.

#The 'brain' dataset shows the average voltage at three electrode sites -- one at the front of the head (fz), one at the middle of the head (cpz), and one toward the back of the head (poz) -- 300-500ms after the presentation of the main clause object ('Mary') in these two sentence types. Your prediction is that when this object is the antecedent of the fronted pronoun (the 'ant' condition; (b) above), the average voltage should be more negative over posterior (i.e., back) electrodes (compared to when it is not the antecedent -- the 'no_ant' condition; (a) above). Evaluate this prediction.

#Provide the relevant descriptive statistics, an illustrative graph, and a write-up of your statistical analyses. [Start with an analysis that includes both 'condition' and 'site'; then determine whether the effect of 'condition' is significant at each site.]


########################

brain=read.csv("brain.csv")
brain
brain$subj=factor(brain$subj)
brain.mean=tapply(brain$MeanV, list(brain$site, brain$condition), mean)
brain.sd=tapply(brain$MeanV, list(brain$site, brain$condition), sd)
brain.mean
brain.sd

interaction.plot(brain$site, brain$condition, brain$MeanV, col=c("blue","red"), xlab = "site", ylab="Mean Voltage", trace.label="", main="Mean Voltage for the antecedent and non-antcedent\nconditions at Fz, CPz, and POz"); grid();

brain$site=factor(brain$site, levels=c("poz","cpz","fz"))

interaction.plot(brain$site, brain$condition, brain$MeanV, col=c("blue","red"), xlab = "site", ylab="Mean Voltage", trace.label="", main="Mean Voltage for the antecedent and non-antcedent\nconditions at Fz, CPz, and POz"); grid();

library(ez)
brain_aov2=ezANOVA(data = brain, dv = .(MeanV), wid = .(subj), within = .(condition, site))
brain_aov2

brain_fz=brain[brain$site=="fz",]
brain_cpz=brain[brain$site=="cpz",]
brain_poz=brain[brain$site=="poz",]

shapiro.test(brain_fz$MeanV[brain_fz$condition=="ant"]-brain_fz$MeanV[brain_fz$condition=="no_ant"])
shapiro.test(brain_cpz$MeanV[brain_cpz$condition=="ant"]-brain_cpz$MeanV[brain_cpz$condition=="no_ant"])
shapiro.test(brain_poz$MeanV[brain_poz$condition=="ant"]-brain_poz$MeanV[brain_poz$condition=="no_ant"])

summary(aov(MeanV~condition+Error(subj/condition),data=brain_fz))
summary(aov(MeanV~condition+Error(subj/condition),data=brain_cpz))
summary(aov(MeanV~condition+Error(subj/condition),data=brain_poz))

## A repeated-measure ANOVA revealed a significant main effect of electrode site (F(2,38)=8.10, p<.01; due to a violation of sphericity (Mauchly's Test for Sphericity (p<.05)), the p-value was Greenhouse-Geisser corrected). There was also a significant main effect of condition (F(1,19)=5.04, p<.05), indicating that the mean voltage in this time window was generally more negative for the antecedent condition. A significant interaction of site and condition was also observed (F(2,38)=4.90, Greenhouse-Geisser corrected p<.05). Although planned comparisons revealed no significant difference in voltage between the antecedent and no-antecedent conditions at fz (F(1,19)=1.14) and cpz (F(1,19)=1.14, p=.15), the antedent condition was significantly more negative at the most posterior electrode site, poz (F(1,19)=12.07, p=.003).

########################



#(7) 'aint.csv'

#You are interested in the use of 'aint' in a dialect of English in the American South and, more specifically, whether its use relates to educational level. You have collected data from 20 subjects on their education level (S_ed) as well as the education levels of their mothers (M_ed) and fathers (F_ed). You have also calculated the percentage of times each subject used 'aint' (as opposed to 'am not'/'is not'/'are not'). Evaluate the relationship between 'aint' use and each of these education level variables. Provide a relevant graph and a write-up of your analysis of this relationship. 

########################

aint=read.csv("aint.csv")
aint

plot(aint$F_ed, aint$percent_AINT)
abline(lm(percent_AINT~F_ed, data=aint))
shapiro.test(aint$percent_AINT)
shapiro.test(aint$F_ed)
cor.test(aint$percent_AINT, aint$F_ed)

#write-up: The relationship between 'ain't' use and the education level of the speaker's father is not significant, r = -0.27.

plot(aint$M_ed, aint$percent_AINT)
abline(lm(percent_AINT~M_ed, data=aint))
shapiro.test(aint$M_ed)
cor.test(aint$percent_AINT, aint$M_ed)

#write-up: There was however a reliable negative correlation between 'ain't' use and the education level of the speaker's mother, r = -0.64, t(18) = -3.53, p < .005. Speakers with better educated mothers use 'ain't' less often.

plot(aint$S_ed, aint$percent_AINT)
abline(lm(percent_AINT~S_ed, data=aint))
shapiro.test(aint$S_ed)
cor.test(aint$percent_AINT, aint$S_ed)
cor.test(aint$percent_AINT, aint$S_ed, method="kendall")

#write-up: There is also a reliable negative correlation between 'ain't' use and the education level of the speaker, tau = -0.63, z = -3.72, p < .001. Speakers with higher education levels use 'ain't' less often.
