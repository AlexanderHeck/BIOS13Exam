#introducing all necessary starting variables
N<-100 #population size of 100
p<-NULL    #introducing the p vector that will save all p values
p[1]<-1/(2*N) #initial frequency of the gene
w<-1.4     #the gene starts out with a fitness advantage since it is >1

#now there will be a for loop that repeats the calculation of pt+1 1000 times
for (t in 1:999) {
  wt<-w*p[t]+1-p[t] #first calculate the mean fitness at time t
  sd<-(1/wt)*sqrt(w/(2*N)*p[t]*(1-p[t])) #calculate the standard deviation of the random factor at time t
  p[t+1]<-w/wt*p[t]+rnorm(1, mean = 0, sd = sd) #now calculate p at time t+1
  if (p[t+1]>=1){
    break
  }
  if (p[t+1]<=0){
    break
  }
}
#now generate the time series plot with x being a time a vector from 1 to the length of p by 1, and y=p 
plot(seq(1, length(p), by=1), p,
     xlab = "Time(t)",
     ylab = "Relative frequency(p)",
     main = "The relative frequency p dependent on time t",
     ylim = c(0,1))
