################## R Codes for #################################
## Introduction to Modern Mathematical Modeling with R: 
#   A User's Manual to Train Mathematical Consultants
#       A Cambridge University Press book by 
#                     SSP Shen
# 
# The R code was written by 
# Samuel Shen, Distinguished Professor
# San Diego State University
# Email: sshen@sdsu.edu
# www.climatestatistics.org
# R code Version 1.0: July 2024 San Diego, California, USA
################################################################


################################################################
#
# Chapter 7: Probabilistic Models
#
################################################################

### The Event-Table Method and Simulation for Two Dice
{r}
#Two-dice simulation
x <- y <- 1:6 #Two dice x and y
m <- 100000 #Simulate m times
l <- 0 #k is used as the counter for a specific event, such as "7"
for (i in 1:m) {
  if(sample(x,1)+sample(y,1) == 7) 
    l=l+1
}
l/m #The simulated probability should be approximately 1/6.


### Buffon's Needle Problem

#setwd("~/sshen/mathmodel")
#png('BuffonNeedlesRandom.png', width=800, height=500) 
#plot.new()
par(mar=c(0.5,0.5,2.5,0.5))
set.seed(101)

d <- 3
l <- 2
plot(c(0,3*d), c(0,0), type='l',xlim=c(0,10),ylim=c(0,10),
     xaxt="n",yaxt="n",bty="n",ann=FALSE)
mtext( "Buffon's Needles on the Evenly Spaced Lines",
       cex=1.5,side=3, line=0) 

p1x <- c(0,0,0,0)
p1y <- c(0,d,2*d,3*d)
p2x <- p1x+3*d
p2y <- p1y

segments(p1x,p1y, p2x,p2y, lwd=2)
arrows(0.1*d,d,0.1*d, 2*d, code=3, length=0.1, angle=10, lwd=1) 
text(0.15*d,1.5*d, 'd', cex=2)

n <- 8 
#8 needles

x1d <- runif(n,1,8)
y1d <- runif(n,1,8)
ang <- runif(n,-pi/2, pi/2)

x2d <- x1d + l*sin(ang)
y2d <- y1d + l*cos(ang)

segments(x1d,y1d, x2d,y2d, col='red', lwd=3)

segments(2.6*d, 1.2*d, 2.6*d + l*sin(pi/6), 
         1.2*d + l*cos(pi/6), col='red',lwd=3)

segments(2.6*d, 1.2*d, 2.6*d + 0.3*l*cos(4*pi/6), 
         1.2*d + 0.3*l*sin(4*pi/6), col='blue', lwd=1)

segments( 2.6*d + l*sin(pi/6), 1.2*d + l*cos(pi/6),
          2.6*d + l*sin(pi/6) + 0.3*l*cos(4*pi/6), 
          1.2*d + l*cos(pi/6) + 0.3*l*sin(4*pi/6), 
          col='blue', lwd=1)

arrows(2.6*d + 0.2*l*cos(4*pi/6),
       1.2*d + 0.2*l*sin(4*pi/6),
       2.6*d + l*sin(pi/6) + 0.2*l*cos(4*pi/6),
       1.2*d + l*cos(pi/6) + 0.2*l*sin(4*pi/6),
       code=3, length=0.1, angle=10, lwd=1, col='blue')

text(2.6*d + 0.2*l*cos(4*pi/6) + 0.5*l*sin(pi/6),
     1.2*d + 0.2*l*sin(4*pi/6) + 0.5*l*cos(pi/6)+ 0.2*d,
     "\u2113", cex=2.5, col='blue') 
#dev.off()


### The Short Needle Problem: l < d

#Figure 7.2
#Buffon needle figure: Short needle l < d
#Buffon needle problem's model

#setwd("~/sshen/mathmodel")
#png('BuffonModelShort.png', width = 800, height = 400) 
#plot.new()
par(mar=c(0.5, 0.5, 2.5, 0.5))
x <- c(0,0)
y <- c(0,2)

#Gap: d = 2
d <- 2 
L <- 2
#Needle length: l = sqrt(L)

plot(x,y,xlim=c(-5,5), lwd=0.1, main="Buffon Needle Model: \u2113 < d", 
     ylim=c(-0.1,2.1), type ="l", xlab='', ylab='', axes=FALSE)

segments(-4,0, 4,0)
segments(-4,2, 4,2) 

a <- 40*pi/180

segments(0,1, sqrt(L)*sin(a), 1+ sqrt(L)*cos(a), lwd=2.5)
arrows(0,0, 0,1,code=3, length=0.1)

text(sqrt(L)*sin(a)+0.2, 1+ sqrt(L)*cos(a), "Q", cex=1.5)
text(0.2,1, "P", cex=1.5)
text(-0.2,0.5, "y", cex=1.5)

arrows(-3,0,-3,2,code=3, length=0.1)
text(-3.2,1,"d",cex=1.5)

arrows(0,1, 0, 1 + sqrt(2)*cos(a), lwd=0.5, length=0.1, code=3) 
text(-0.55,1.7,expression("\u2113cos"*theta), cex=1.5)

segments(0,1 + sqrt(L)*cos(a), sqrt(L)*sin(a), 1+ sqrt(L)*cos(a), 
         lwd=0.5) 
x1 <- seq(0,0.37, len=90)

lines(x1,1+sqrt(0.6^2-x1^2), type="l",lwd=0.5)

text(0.2,1.65,expression(theta), cex=1.4) 
text(0.7,1.7,"\u2113", cex=1.5) 
text(2.75,0.75, cex=1.5, expression("Needle Crossing: "*y*" + \u2113"*cos*theta> d)) 
#dev.off()

#Figure 7.3
#setwd("~/sshen/mathmodel")
#png('BuffonProbabilityShort.png', width=800, height=600) 
#plot.new()

par(mar=c(0.5,0.5,2.5,0.5))
d <- 2
L <- 2
l <- sqrt(L)
x3 <- seq(-pi/2,pi/2, len=1000)
y3 <- d-l*cos(x3) 

plot(x3,y3,xlim=c(-0.6*pi,0.6*pi), lwd=1.5,
     main="Buffon Problem's Geometric Probability Diagram: \u2113 < d", 
     ylim = c(0,d+0.4), type = "l", xlab='', ylab='', axes = FALSE)

segments(-pi/2,d, pi/2,d, lwd=1.5) 
segments(pi/2,0, pi/2,d, lwd=0.5) 
segments(-pi/2,0, -pi/2,d, lwd=0.5) 
segments(-pi/2,0, pi/2,0, lwd=0.5) 

arrows(-0.6*pi,0, 0.6*pi,0,code=2, length=0.1) 
arrows(0,0, 0,d+0.5, code=2,length=0.1) 

text(-0.1,d+0.4, "y", cex=2) 
text(pi/2+0.35,0.1, expression(theta), cex=2) 

k1 <- 21
x6 <- x7 <- seq(-pi/2, pi/2, len=k1)
y6 <- rep(d,k1)
y7 <- d-l*cos(x6)
s <- 1:k1

segments(x6[s],y6[s],x7[s],y7[s], lwd=0.5)

text(-0.1,2.1,"d", cex=2) 
text(0.2,0.7*d,"Ac", cex=2) 
text(-0.17,d-l-0.1,"d - \u2113", cex=2) 
text(pi/4 +0.25,0.7, cex=2,
     expression(""*y*"= d - \u2113"*cos*theta)) 

text(-0.1,0.1,"0", cex=2)
text(-pi/2 + 0.1, 0.18,expression(-frac(pi,2)), cex=2) 
text(pi/2 + 0.1, 0.18,expression(frac(pi,2)), cex=2) 

#dev.off()


### The Long Needle Problem: l > d

#Figure 7.4

#Buffon's needle model: The case of long needles l > d 
#setwd("~/sshen/mathmodel")

#png('BuffonModelLong.png', width=800, height=400) 
#plot.new()

#Gap: d = 2
d <- 2
#Needle length: l = sqrt(L)
L <- 5

par(mar=c(0,0.5,2.5,0.5))
x <- seq(0,sqrt(L), len = 1000)
y <- (2/sqrt(L))*x

#plot(x,y,type ="l", xaxt='n', yxat='n') 
plot(x,y,xlim=c(-5,5), lwd=1.5,
     main="Buffon's Needle Model: Long Needles \u2113 > d",
     ylim=c(0,3),type ="l", xlab='', ylab='', axes=FALSE) 

segments(-4,0, 4,0)
segments(-4,2, 4,2)
segments(0,0.5, 1.5,3.1, lwd=2.5)

arrows(0,0, 0,0.5,code=3, length=0.1)
text(-0.2,0.25, "y", cex=1.5)
text(-0.2,0.5, "P", cex=1.5) 
text(1.5+0.2,3.1-0.1, "Q", cex=1.5) 

arrows(-3,0,-3,2,code=3, length=0.1) 
text(-3.2,1,"d",cex=1.5)

arrows(0,0.5, 0,3.1, lwd=0.5, length=0.1, code=3) 
text(-0.35,2.4,expression("\u2113 cos"*theta)) 

segments(0,3.1, 1.5,3.1, lwd=0.5)
x1 <- seq(0,0.28, len=90) 
lines(x1,0.5+sqrt(0.6^2-x1^2), type="l",lwd=0.5) 
text(0.2,1.2,expression(theta), cex=1.5) 

x2 <- seq(0,0.21, len=90) 
lines(x2,sqrt(0.3^2-x2^2), type="l",lwd=0.5)

text(0.2,0.4,expression(alpha), cex=1.5) 
text(0.8,2.4,"\u2113", cex=1.5) 
text(1.6,1.6,"\u2113",cex=1.5)
text(2.5,2.2, cex=1.5,
     expression("Crossing: "*y*"+ \u2113"*cos*theta> d))
#dev.off()


#Figure 7.5
#Buffon's needle's geometric probability diagram: 
#Long needles l > d 
#setwd("~/sshen/mathmodel")

#png('BuffonProbabilityLong.png', width=800, height=600) 
#plot.new()

par(mar=c(0.0,0.0,2.5,0.5))
d <- 2
l <- 2.1
x3 <- seq(acos(d/l),pi/2, len=1000) 
y3 <- d-l*cos(x3) 
plot(x3,y3,xlim=c(-0.6*pi,0.6*pi), lwd=1.5,
     main="Buffon's Needle Problem:
     Geometric Probability Diagram for Long Needles \u2113 > d",
     ylim=c(-0.3,d+0.5),type ="l", xlab='', ylab='', axes=FALSE)

x4 <- seq(-pi/2, -acos(d/l), len=1000)
y4 <- d-l*cos(x4)
lines(x4,y4, type="l",lwd=1.5)

segments(-pi/2,d, pi/2,d, lwd=1.5)
segments(-acos(d/l),0, acos(d/l),0, lwd=1.5)
segments(pi/2,0, pi/2,d, lwd=0.5) 
segments(-pi/2,0, -pi/2,d, lwd=0.5) 
segments(-pi/2,0, pi/2,0, lwd=0.5) 

arrows(-0.6*pi,0, 0.6*pi,0,code=2, length=0.1) 
arrows(0,0, 0,d+0.5, code=2,length=0.1) 

text(-0.1,d+0.3, "y", cex=2)
text(0.06,d+0.1, "d", cex=2) 
text(pi/2+0.2,0.15, expression(theta), cex=2)

k1 <- 15
x6 <- x7 <- seq(-acos(d/l), acos(d/l), len=k1) 
y6 <- rep(0,k1)
y7 <- rep(d,k1)
s <- 1:k1
segments(x6[s],y6[s],x7[s],y7[s], lwd=0.5)

k2 <- 10
x8 <- x9 <- seq(acos(d/l), pi/2, len=k2)
y8 <- d-l*cos(x8)
y9 <- rep(d,k2)
s <- 1:k2
segments(x8[s],y8[s],x9[s],y9[s], lwd=0.8)

k3 <- 10
x3 <- x4 <- seq(-pi/2,-acos(d/l), len=k2)
y3 <- d-l*cos(x3)
y4 <- rep(d,k2)
s <- 1:k3
segments(x3[s],y3[s],x4[s],y4[s], lwd=0.8)

text(0.1,0.5*d,expression(A[x]), cex=2) 
text(acos(d/l) + 0.6,0.8*d,expression(A[c]), cex=2)
text(-pi/2,-0.25,expression(-frac(pi,2)), cex=1.8) 
text(pi/2,-0.25,expression(frac(pi,2)), cex=1.8) 
text(-acos(d/l),-0.15,expression(-alpha), cex=2) 
text(acos(d/l),-0.15,expression(alpha), cex=2) 
text(0,-0.15,"0", cex=2)
#dev.off()



### Computer Simulation of the Buffon's Needle Problem

#Buffon's needle simulation: short needle 
d <- 4
l <- 2
k <- 0
n <- 10000
for (i in 1:n) {
  if((runif(1,0,d)+l*cos(runif(1,-pi/2,pi/2))) >= d) 
    k=k+1} 
#Here runif(1,0,d) is y, and runif(1,-pi/2,pi/2) is \theta.
k/n # the simulation answer.
#This can be calculated by the derived formula 
(2/pi)*(l/d) # the exact answer.

#The long needle simulation code is the same as the short needle. 
#One simply changes the values of d and l
d <- 2
l <- 4
k <- 0
n <- 10000
for (i in 1:n) {
  if((runif(1,0,d)+l*cos(runif(1,-pi/2,pi/2))) >= d)
    k=k+1} 
k/n # the simulation answer.
#This can be calculated by the derived formula
(2/pi)*(l/d + acos(d/l)-sqrt((l/d)^2-1)) # the exact answer



### User Monte Carlo Simulation to Estimate the Volume of an n-ball

rm(list=ls())
#remove the R console history 

N <- 10000
x <- matrix(runif(2*N, -1,1),ncol=2)
k <- 0
for(i in 1:N){
  if((t(x[i,])%*%x[i,]) < 1) 
  {k=k+1
  x[k,]=x[i,]}
}

#r=k/N is the ratio of points inside the ball to the 
#total number of points. r*2^2 is the ball's volume 
#since 2^2 is the total volume of the square of side 
#equal to the diameter of the disk.

(k/N)*2^2 # approximate value of pi
k 

#Figure 7.6
#Plot all the 10000 points x[1:N,] inside the square 
par(mar=c(4.5,4.5,3,0.5))
plot(x,pch=19, cex=0.2,
     xlim=c(-1,1),ylim=c(-1,1), xlab=expression("x"[1]), 
     ylab=expression("x"[2]),
     cex.lab=1.5, cex.axis=1.5,
     main="10000 Random Points on a 2D Square")
#Plot 7853 red points y[1:k,] inside the unit circle 
points(x[1:k,],pch=19, cex=0.3,col="red")
grid(nx = NULL, ny = NULL)

# Example 7.2
#Calculate for volume for n-dim unit ball
N <- 100000
n <- 8
x <- matrix(runif(n*N, -1,1),ncol=n)
k <- 0
for(i in 1:N){
  if((t(x[i,])%*%x[i,]) < 1) {
    k=k+1}} 
k

(k/N)*2^n # is the volume

#The exact answer is pi^4/24 R^8 = 4.0587 
#Or use the general formula for B_8
n <- 8
pi^(n/2)/gamma(n/2 +1)



### Use Monte Carlo Simulation for Numerical Integration

## Example 7.3
#MC for an integral
f <- function(x){x^2} # Define a function 
f

function(x){x^2}
x <- runif(1000, 1,3) # Using 1,000 samples
(3-1)*mean(f(x))

x <- runif(1000000,1,3) # Using 1,000,000 samples 
(3-1)*mean(f(x))

integrate(f,1,3) # R code for numerical integration 


## Example 7.4
#int[exp(-x^2)/(1 + x^2), -1,2]
f2 <- function(x){exp(-x^2)/(1+x^2)} 
f2

function(x){exp(-x^2)/(1+x^2)} 
x <- runif(1000,-1,2) 
(2-(-1))*mean(f2(x))

integrate(f2,-1,2)


## Example 7.5
rm(list=ls())
#remove the R console history set.seed(233)
N <- 100000
x <- matrix(runif(5*N, -1,1),ncol=5) 
y <- matrix(5,ncol=5,nrow=N)
k <- 0
for(i in 1:N){
  if((t(x[i,])%*%x[i,]) < 1) {
    k=k+1 
    y[k,]=x[i,]}
}
k # number of points inside the unit ball B5 in the 5D space 
#y[1:k,] are points inside the unit ball

r1 <- y[1:k,1]
r2 <- y[1:k,2]
r3 <- y[1:k,3]
r4 <- y[1:k,4]
r5 <- y[1:k,5]

f <- function(x1,x2,x3,x4,x5){1 + x1^2 + 
    x2^2 + x3^2 + x4^2 + x5^2}
n <- 5

V5 <- pi^(n/2)/gamma(n/2 +1) # Compute the volume of B5
V5*mean(f(r1,r2,r3,r4,r5)) #volume of B5 times the mean value of the function
# The approximate value of the 5D integral



### Markov Chains

P <- matrix(c(1/3,.7,1,1/3,.3,0,1/3,0,0),
            nrow=3)
P

v1 <- matrix(c(1/3,1/3,1/3),nrow=1)
v2 <- v1%*%P 
v2

v3 <- v2%*%P 
v4 <- v3%*%P 
v4

matrix.power <- function(A, n) { 
  e <- eigen(A)
  M <- e$vectors # matrix for changing basis 
  d <- e$values # eigenvalues
  return(M %*% diag(d^n) %*% solve(M))
}

v4 <- v1%*%P^3 
v4 <- v1%*%matrix.power(P,3)
v4

B10 <- matrix.power(P,10)
B10

B20 <- matrix.power(P,20) 
B20

m1 <- t(P)-diag(3) 
m1

A <- matrix(c(-2/3,1/3,1,0.7,-0.7,1,1,0,1), 
            nrow=3) 
b <- matrix(c(0,0,1),nrow=3)
v <- solve(A,b) 
v

v21 <- v1%*%B20 
v21

P <- matrix(c(1/3,.7,1,1/3,.3,0,1/3,0,0),
            nrow=3) 
P

B <- matrix(c(rep(1,3),rep(0,3*(3-1))), 
            by=T,ncol=3) 
B

y <- matrix(c(1,0,0),by=T) 
y

D <- t(P)-diag(3)+B 
D

u <- solve(D,y) 
u

B20



### Example 2: Order of Fish Tanks

# P(d=0) <- 0.368 # (no demand for a week)
# P(d=1) <- 0.368
# P(d=2) <- 0.184
# P(d=3) <- 0.061
# P(d>3) <- 0.0169

F <- matrix(c(0.368, 0.368, 0.184,
              0, 0.368, 0.368, 0.632, 0.264, 0.448), nrow =3)
F

m2 <- matrix.power(F,100)
m2

#The event of demand greater than supply (d > x) includes the following cases:
#x=1, d=2,3,>3 
#x=2, d=3,>3 
#x=3, d>3

#Markov chain problem: Given the weather transition probability 
#from one condition at a day to the next day, find the steady state 
# of the probability of each weather condition.
#install.packages("expm")
#install.packages("markovchain")
#install.packages("diagram")
#install.packages("pracma")
library(expm)
library(markovchain)
library(diagram)
library(pracma)

stateNames <- c("Rain","Nice","Snow")
Oz <- matrix(c(.5,.25,.25,.5,0, .5,.25,.25,.5),
             nrow=3, byrow=TRUE) 

row.names(Oz) <- stateNames
colnames(Oz) <- stateNames
Oz

#The transition matrix' row sums are one, 
#but column sums do not need to be one

#Plot the Markov chain diagram
plotmat(Oz,pos = c(1,2), lwd = 1, box.lwd = 2,
        cex.txt = 0.8,
        box.size = 0.1,
        box.type = "circle", box.prop = 0.5,
        box.col = "light yellow", 
        arr.length=.1, arr.width=.1,self.cex = .4,
        self.shifty = -.01,
        self.shiftx = .13,
        main = "Markov Chain for Weather Transition")

#This diagram shows that nice weather cannot continue.
Oz3 <- Oz %^% 3
round(Oz3,3)

Ozs <- Oz %^% 30
Ozs #Gives the steady state probability
#The rain and snow days probability is 0.4 and nice day's 0.2

u <- c(1/3, 1/3, 1/3)
round(u %*% Oz3,3)

#After three iterations, the initial condition is forgotten 
#and approaches the steady state.
round(u %*% Oz3,10)
#after 10 iterations, the initial conditions are completely forgotten

#Another example of Markov chain
#Fish Tank Order Markov Chain from Mark Meerschaert's book 
#https://www.stt.msu.edu/~mcubed/
#Inventory policy for a store: Order 3 tanks when no tanks left in the store
#Practice: The store can meet the demand by paying an extra cost and 
# acquiring the needed tanks from a sister store. Thus, a customer
# can actually buy 4 or more tanks from this store. However, acquiring 
# a tank from a sister store cuts the profit to zero for this tank. 
# Thus, the profitable sale is that the store has enough tanks to
# meet the customers' needs.

#Historical data
#P(d=0)=0.368 (no demand for a week)
#P(d=1)=0.368
#P(d=2)=0.184
#P(d=3)=0.061
#P(d>3)=0.0169

#The above data assumes that the store can meet the demand by
# acquiring the needed tanks from a sister store. Thus, a customer 
# can actually buy 5 tanks from this store.
#The transition matrix is the probability of F(i,j)=P(x=i=>x=j) 
# where x is the number of tanks in store.
#For example, F(1,3) means that the store has one tank but changes 
# to three tanks next week. This means that the store must sell
# at least one tank so that the store can order three for next week. 
#Selling at least one means the union of d=1, d=2, d=3, and d>3 
#P(d=1, d=2, d=3, and d>3)
# = P(d=1)+P(d=2) + P(d=3) + P(d>3)
# = 0.368 +0.184 + 0.061 + 0.019
# = 0.632, i.e., F(1,3)=0.632
#F(1,2) is never possible, hence F(1,2)=0
#F(1,1)=0.368 means selling no sale
#Transition matrix can be derived from the above data 
#F = matrix(c(0.368, 0.368, 0.184, 0, 0.368, 0.368, 0.632, 0.264, 0.448),nrow=3)

F <- matrix(c(0.368, 0,0.632, 0.368,0.368,0.264, 0.184, 0.368, 0.448),
            byrow=TRUE, nrow=3) 
F

#This is the transition matrix
#Define a function o compute the power of a matrix 
#The transition matrix' row sums are one,
# but column sums do not need to be one
#Plot the Markov chain diagram
plotmat(F,pos = c(1,2),
        lwd = 1, box.lwd = 2,
        cex.txt = 0.8,
        box.size = 0.1,
        box.type = "circle", box.prop = 0.5,
        box.col = "light yellow",
        arr.length=.1,
        arr.width=.1,
        self.cex = .4,
        self.shifty = -.01,
        self.shiftx = .13,
        main = "Markov Chain for Fish Tank Ordering")

matrix.power <- function(A, n) {
  e <- eigen(A)
  M <- e$vectors # matrix for changing basis 
  d <- e$values # eigenvalues
  return(M %*% diag(d^n) %*% solve(M))
}
m2 <- matrix.power(F,100)
m2
Re(m2[1,]) #Real parts=>The steady state probability
#This means P(x=1)=0.2848349 is the probability of having 
# one tank at a the store. Similarly, P(x=2) = 0.2631808 
# P(x=3)=0.4519844. Nearly half of the time the store has 
# three tanks.

# The event of demand greater than supply (d>x) includes the following cases:
#x =1, d=2,3, >3
#x=2, d=3, d>3
#x=3, d>3
#Thus, the probability of P(d>x) is
#P(d>x) = sum_{i=1}^3 P(d>x|x=i)P(x=i)
# = (0.184 + 0.061 + 0.019) x 0.285 + (0.061 + 0.019) x 0.263 + 0.019 x 0.452 
# = 0.104868.

P <- (0.184 + 0.061 + 0.019)*0.285 + (0.061 + 0.019) * 0.263 + 0.019 * 0.452 
P
#Thus, the probability of demand greater than supply is 10%.
#This is a reasonably small probability for practical store 
# operation.

#One can use iteration to find the steady state
#Use Markov chain simulations by iteration
v <- matrix(rep(0, 150), nrow=3) 
#Define the data storage space 
v[1,1] <- 1 #Define an initial condition
v[,1]
#P(x=1)=0 at the beginning as an assumed initial condition

#Making 49 iterations
for (i in 1:49) {
  v[,i+1]=v[,i]%*%F
}
v[,50]

#The limit of v_i+1 = v_i F is the steady state v with v=vF 
#This is the same as F^49 since v_50=v_1F^49
