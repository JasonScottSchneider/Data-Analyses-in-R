#**For this homework, I would like you to think about and play around with probability distributions. It is necessary to understand these distributions and how they relate to statistical significance (i.e., passing a threshold that allows you to reject the null hypothesis).**

#(1) Your friend is convinced that he has extra-sensory perception (ESP), and you are convinced that he does not. So you design a test. You shuffle a full deck of cards (i.e., 52 cards) ; you ask him to say what the top card is; you then turn the card over to see if he is correct. After each guess, you put the card back in the deck and repeat the procedure. You do this a bunch of times, and he only gets a couple right. So you say, "See. You don't have ESP. If you were psychic, you would have been able to tell me the correct card each time." To which he responds, "Well, I didn't say it was perfect. But I still have ESP. After all, I got a few right." So you play again. This time you play the game 200 times, and you carefully record the result each time. (You and your friend have far too much time on your hands!) How many times would your friend have to get the correct answer in order for you to reject the null hypothesis that he does not have (helpful) ESP? (Hint: You will first have to calculate the probability of success on a single trial. Obviously, this is not .5, as it is when you flip a coin.)

1/52 #calculates probability of success on a single trial
plot(0:200, dbinom(0:200, 200, 0.01923077), type = "h", xlab="frequency of correct", ylab="probability of frequency") #plotted just for fun
pbinom(7, 200, 0.01923077, lower.tail=FALSE)

qbinom(.05, 200, 0.01923077, lower.tail=FALSE)

#Your friend would have to get 8 or more correct in order to reject the null hypothesis (that he does not have helpful ESP).


#(2) Your other friend sees you playing this game and says that he wants to try next because he has an especially weird kind of ESP. It is more like a little voice in his head that tells him the answers to things. (You need to get some new friends!) Some days, his little voice is helpful, and tells him the correct answers. But other days, the voice messes with him and only gives him the wrong answers. Could you use your test to investigate this? How many times would your friend have to get the correct answer in order for you to reject the null hypothesis that he does not have this weird ESP? (Remember, you don't know whether his voice is being helpful or not on this particular day.)

2*pbinom(0, 200, 0.01923077)
2*pbinom(8, 200, 0.01923077, lower.tail=FALSE)

qbinom(.05/2, 200, 0.01923077)
qbinom(.05/2, 200, 0.01923077, lower.tail=FALSE)

#Your other friend would have to get none or 9 or more correct in order to reject the null hypothesis (that he does not have helpful/unhelpful ESP).


BONUS:

#Evaluate the observed frequency of the word "senator" in the Brown corpus based on the probability of this word occurring in the CELEX database [Brown corpus frequency = 40 (corpus size = 1000000); CELEX database frequency = 267 (corpus size = 18580121)].

#(i) What is the probability of obtaining a value at the observed frequency of 40 or higher in the Brown corpus?
#(ii) What does this say about the frequency of the word "senator" in the Brown corpus?

p=267/18580121 #calculates the probability of occurrence of the word based on CELEX, sets this as a variable p
pbinom(39, 1000000, p, lower.tail=FALSE)

#The output of this code indicates that it is extremely unlikely that 'senator' would appear 40 or more times in the Brown corpus (again, based on what we know about the frequency of occurrence for this word in the CELEX corpus).

#The frequency of the word 'senator' in the Brown corpus differs significantly from the expected frequency based on the CELEX database. According to a binomial test, the probability of obtaining a value at the observed frequency of 40 or higher is very low (p<.001); people are using 'senator' in the Brown corpus more than would be expected.