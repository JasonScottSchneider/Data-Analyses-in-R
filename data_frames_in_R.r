#PS2: Dealing with data frames in R.

#(a) Read the 'masked priming.csv' into R.
#(b) Create a new column in this data frame called 'newsubj' which has each subj#+100; save this column so that the values are treated as characters, not as real numbers.
#(c) From this revised data frame, create a new data frame that includes the data only for those items (i) that were responded to correctly (0 in the 'error' column) and (ii) that have RTs greater than 300ms.
#(d) Write a line of code that tells you how many observations were eliminated when you created the revised data frame in (c).
#(e) Based on the data frame you create in (c), create two new data frames -- one for high-frequency (HF) words and another for low-frequency (LF) words.
#(f) Save your three new data frames -- the one created in (c) and the two created in (e) -- as new .csv files.



mp=read.csv("masked priming.csv", header=T)
mp$newsubj=mp$subj+100
mp$newsubj=as.character(mp$newsubj)
mp.revised=mp[mp$error=="0" & mp$rt>300, ]
length(mp$subj)-length(mp.revised$subj)
mp.revised.HF=mp.revised[mp.revised$freq=="HF", ]
mp.revised.LF=mp.revised[mp.revised$freq=="LF", ]
write.csv(mp.revised, "mp.revised.csv")
write.csv(mp.revised.HF, "mp.revised.HF.csv")
write.csv(mp.revised.LF, "mp.revised.LF.csv")