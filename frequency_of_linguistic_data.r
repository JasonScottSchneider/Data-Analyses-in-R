##PS5

#(1) In the 'auxiliaries' data set (in languageR), determine whether Dutch auxiliary choice (Aux) relates to whether the verb is regular or irregular (Regularity). Generate representative descriptive statistics and an illustrative graph. In a few sentences, discuss the pattern of data with respect to your research question.

library(languageR)
aux=table(auxiliaries$Aux, auxiliaries$Regularity)
aux
aux_perc=prop.table(table(auxiliaries$Aux, auxiliaries$Regularity), margin=1)
aux_perc

plot((auxiliaries$Regularity~auxiliaries$Aux), ylab="Verb Type", xlab="Auxiliary")

##Write-up: The data indicate that 'hebben' is the most frequent auxiliary and that it is preferentially associated with regular verbs. The auxiliaries 'zijn' and 'zijnheb', on the other hand, appear to be preferentially associated with irregular verbs.


#(2) Use the '03-1_uh(m).txt' dataset to determine whether disfluency length is influenced by the type of disfluency. Generate representative descriptive statistics (minimally, mean and sd) and a graph to examine this question. In a few sentences, discuss the pattern of data with respect to your research question.

UHM=read.table("03-1_uh(m).txt", header=T)
tapply(UHM$LENGTH, UHM$FILLER, mean)
tapply(UHM$LENGTH, UHM$FILLER, sd)                
boxplot(UHM$LENGTH~UHM$FILLER, notch=T, ylim=c(0, 1600)); grid()                
text(seq(levels(UHM$FILLER)), tapply(UHM$LENGTH,  UHM$FILLER, mean), "X")

##Write-up: All of the disfluency types ('silence', 'uh', and 'uhm') have comprable lengths. In other words, the disfluency length does not appear to be influenced by the type of disfluency.

                 
#(3) Use the 'verbs' dataset (in languageR) to determine whether the length of the theme (LengthOfTheme) relates to the realization of the recipient (RealizationOfRec) and the animacy of the recipient (AnimacyOfRec). Generate representative descriptive statistics (minimally, mean and sd) and a graph to examine this question (you might want to try an 'interaction.plot'; see the class notes and the text book readings). In a few sentences, discuss the pattern of data with respect to your research question.

tapply(verbs$LengthOfTheme, list(verbs$RealizationOfRec, verbs$AnimacyOfRec), mean)
tapply(verbs$LengthOfTheme, list(verbs$RealizationOfRec, verbs$AnimacyOfRec), sd)
interaction.plot(verbs$RealizationOfRec, verbs$AnimacyOfRec, verbs$LengthOfTheme, ylim=range(verbs$LengthOfTheme)); grid()

##Write-up: Sentences with NP recipients are associated with longer themes than sentences with PP recipients. Also, sentences with animate recipients generally have longer themes than those with inanimate recipients.
