# Problem Set 4 #

#Find the following information for the reaction times (rt) to high-frequency (HF) and low-frequency (LF) words in the 'masked priming.csv' data set. Only analyze the data for words that were correctly identified (i.e., with '0' in the error column). (A good way to do this might be to first subset the data so that only correct responses are included in the data frame. You might then create new data frames for the HF and LF words.)

# (1) Find the mean, median, range, interquartile range, standard deviation, variance, variation coefficient, standard error of the mean, and 95% confidence intervals of the mean.
# (2) Create a representative boxplot for the reaction times to HF words and another for reaction times to LF words.


mp=read.csv("masked priming.csv", header=T)
mp_cor=mp[mp$error=="0", ]
mp_cor_HF=mp_cor[mp_cor$freq=="HF",]
mp_cor_LF=mp_cor[mp_cor$freq=="LF",]

mean(mp_cor_HF$rt)
median(mp_cor_HF$rt)
range(mp_cor_HF$rt)
diff(range(mp_cor_HF$rt))
IQR(mp_cor_HF$rt)
sd(mp_cor_HF$rt)
var(mp_cor_HF$rt)
sd(mp_cor_HF$rt)/mean(mp_cor_HF$rt)
sd(mp_cor_HF$rt)/sqrt(length(mp_cor_HF$rt))
t.test(mp_cor_HF$rt, conf.level=0.95)$conf.int

boxplot(mp_cor_HF$rt, notch=T, main="Reaction Times to High-Frequency Words")
text(1, mean(mp_cor_HF$rt), "+")


mean(mp_cor_LF$rt)
median(mp_cor_LF$rt)
range(mp_cor_LF$rt)
diff(range(mp_cor_LF$rt))
IQR(mp_cor_LF$rt)
sd(mp_cor_LF$rt)
var(mp_cor_LF$rt)
sd(mp_cor_LF$rt)/mean(mp_cor_LF$rt)
t.test(mp_cor_LF$rt, conf.level=0.95)$conf.int

boxplot(mp_cor_LF$rt, notch=T, main="Reaction Times to Low-Frequency Words")
text(1, mean(mp_cor_HF$rt), "+")
