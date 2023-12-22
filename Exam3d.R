#Exercise 3 d), isoclines
#First the variables are introduced and values assigned according to the exercise
b<-1
g<-0.5
µJ<-0.3
µ0<-0.2
c<-0.01

#first introduce a vector of A values
A<-seq(0,400, by=1)

#Calculate the values of the Juvenile Isocline with the formula obtained in the exercise
JI<-(A*b)/(g+µJ)

#now plot the juvenile isocline as a blue line
plot(A,JI, type='l', col='blue',
     xlab='Adults',
     ylab='Juveniles')

#calculate the values of the adult isocline with the formula obtained in the exercise
AI<-(µ0*(1+(c*A))*A)/g

#plot the Adult isocline using the lines command
lines(A,AI, col='red')

#add a legend using the legend command
legend("bottomright", legend = c("Juvenile isocline","Adult isocline"), 
       col = c('blue', 'red'),
       lty = 1)

