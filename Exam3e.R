#First all necessary variables are introduced
b<-1
g<-0.5
µJ<-0.3
µ0<-0.2
c<-0.01
#then iclude  all above parameters in one list
pars<-list(b=b,g=g,µJ=µJ,µ0=µ0,c=c)
#to use the ode function, the deSolve package need to be loaded
library(deSolve)
#The two differential equations are introduced into the JAfunc function
JAfunc<-function(t, JA, pars){
  J<-JA[1]
  A<-JA[2]
  dJdt<-pars$b*A-pars$g*J-pars$µJ*J
  dAdt<-pars$g*J-pars$µ0*(1+pars$c*A)*A
  return(list(c(dJdt,dAdt)))
}
timevec<-seq(0,40, by=0.1) #introducing the time vector
JA0<-c(J=300, A=400) #setting the initial condition as J and A = 50
results<-ode(func = JAfunc, times = timevec, y = JA0, parms = pars)

#plot the results first as a function of time
plot(results[,1], results[,2], type = 'l', col='green', ylim=c(150,450),
     ylab = "Number of Juveniles and Adults",
     xlab = "time",
     main = "Number of Juveniles and Adults dependent on time")
lines(results[,1], results[,3], col='purple')
#add a legend using the legend command
legend("topright", legend = c("Number of Juveniles","Number of adults"), 
       col = c('green', 'purple'),
       lty = 1)

#now in the second part, the isoclines are being plotted with the same R code as in exercise 3d
A<-seq(0,400, by=1)

#Calculate the values of the Juvenile Isocline with the formula obtained in the exercise
JI<-(A*b)/(g+µJ)

#now plot the juvenile isocline as a blue line
plot(A,JI, type='l', col='blue',
     xlab='Adults',
     ylab='Juveniles',
     main = "Juveniles depend on Adults")

#calculate the values of the adult isocline with the formula obtained in the exercise
AI<-(µ0*(1+(c*A))*A)/g

#plot the Adult isocline using the lines command
lines(A,AI, col='red')

#add a legend using the legend command
legend("bottomright", legend = c("Juvenile isocline","Adult isocline","Simulation results"), 
       col = c('blue', 'red', 'black'),
       lty = 1)
#and now the results of the ode function are plotted on top of the isoclines
lines(results[,3], results[,2])
