source("~/BIOS13Exam/Exam4d.R") #sources the previous exercise file to load the 
#function created in exercise 4d, please adapt the save path as needed
w<-seq(0.9, 1.1, by=0.01) #introduce w as the range of the values of fitness of the gene
p20<-NULL #introduce both p20 and p200 to save the fixation probabilities
p200<-NULL

par(mfrow=c(1,2)) #create a new plotting window that allows to plot both graphs next to each other

#First run the analysis and plot the probability of fixation with N=20
for (i in 1:length(w)){         #for loop that carries out the fixation function for every value of w
p20[i]<-fixation(20, w[i])
}
plot(w,p20,
     xlab = "Gene fitness(w)",
     ylab = "Probability of fixation",
     main = "N=20",
     ylim = c(0,0.3)) #for better comparability both plots will have the same y limits (multiple runs have shown that p < than 0.3 in both cases)

#Then run the same analysis and make a plot for N=200
for (i in 1:length(w)){         #for loop that carries out the fixation function for every value of w
  p200[i]<-fixation(200, w[i])
}
plot(w,p200,
     xlab = "Gene fitness(w)",
     ylab = "Probability of fixation",
     main = "N=200",
     ylim = c(0,0.3))