fixation<-function(N,w){
p<-NULL    #introducing the p vector that will save all p values
p[1]<-1/(2*N) #initial frequency of the gene
count<-0 #introduce the count variable that counts all fixation events

for (i in 1:1000){
#the following for loop was taken from Exercise 4c
for (t in 1:999) {
  wt<-w*p[t]+1-p[t] 
  sd<-(1/wt)*sqrt(w/(2*N)*p[t]*(1-p[t])) 
  p[t+1]<-w/wt*p[t]+rnorm(1, mean = 0, sd = sd) 
  if (p[t+1]>=1){
    count<-count+1
    break
  }
  if (p[t+1]<=0){
    break
  }
}
}
return(count/1000) #returns the probability of fixation as the number of fixation events/1000
}