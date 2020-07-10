#PS9

#You test children with specific language impairment (SLI), age-matched controls (AGE_mat), and MLU-matched controls (MLU_mat) on a particular grammatical morpheme type in Language X. You want to know whether the children with specific language impairment produce this form less accurately than each of the control groups. The dataset 'SLI' presents the mean accuracy rate (i.e., the percent correct) on this form for each participant in this experiment.

#The answer to this question should include 
	#(i) descriptive statistics and an illustrative graph
	#(ii) the complete set of code that you used to analyze these data (annotated to justify your analyses!)
	#(iii) a very short write-up of the results of your analyses

SLI=read.csv("SLI.csv", header=T)
tapply(SLI$accuracy, SLI$group, mean)
tapply(SLI$accuracy, SLI$group, sd)
boxplot(SLI$accuracy~SLI$group, names=c("AGE-MATCHED", "MLU-MATCHED", "SLI"), col=c("lightblue", "blue", "orange"), notch=T);
text(1:3, tapply(SLI$accuracy, SLI$group, mean), "X")
text(1:3, c(35, 10, 80), labels=paste("mean=\n", round(tapply(SLI$accuracy, SLI$group, mean), 2), sep=""), col=c("lightblue", "blue", "orange"))
title(main="Accuracy Rates for the SLI group\n and their age-matched and MLU-matched controls", ylab = "mean accuracy")
tapply(SLI$accuracy, SLI$group, shapiro.test)
bartlett.test(SLI$accuracy~SLI$group)
shapiro.test(residuals(lm(SLI$accuracy~SLI$group)))
summary(aov(SLI$accuracy~SLI$group))
t.test(SLI$accuracy[SLI$group=="SLI"], SLI$accuracy[SLI$group=="AGE_mat"], var.equal=TRUE)
t.test(SLI$accuracy[SLI$group=="SLI"], SLI$accuracy[SLI$group=="MLU_mat"], var.equal=TRUE)

#There was a significant difference among the mean accuracy rates for the three groups, F(2,87)=95.60, p < .001. Planned comparisons showed the SLI group produced form X less accurately than both their age-matched controls (t(58)=15.37, p<.001) and MLU-matched controls (t(58)=8.47, p<.001).


#If we did not have planned comparisions....

t.test(SLI$accuracy[SLI$group=="SLI"], SLI$accuracy[SLI$group=="AGE_mat"], var.equal=TRUE)
t.test(SLI$accuracy[SLI$group=="SLI"], SLI$accuracy[SLI$group=="MLU_mat"], var.equal=TRUE)
t.test(SLI$accuracy[SLI$group=="MLU_mat"], SLI$accuracy[SLI$group=="AGE_mat"], var.equal=TRUE)
#(with a Bonferroni-corrected alpha of .05/3, or .0167)

TukeyHSD(aov(SLI$accuracy~SLI$group))
