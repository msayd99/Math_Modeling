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
# Chapter 6: Mathematical Modeling By Calculus
#
################################################################


### Optimal Dimensions of Food Cans
pfr <- function(r) {6*pi*r^3 + 9.4*pi*r^2 -1456.88} 
uniroot(pfr,c(0,8))


### Optimal Production Level of Oil
x <- seq(7.5,11.0,length=100)
rev <- function(x) {10^(-3)*30*x*(10000/(x+1)^2)*(cos(0.5*(x-8.5)))} 
plot(x,rev(x),type="l", 
     main="Monthly Oil Revenue vs. Oil Production for a Country",
     xlab="Oil Production [Mbbl/Day]",ylab="Billion USD" )
grid(nx = NULL, ny = NULL)

#Search for the maximum revenue
max(rev(x))

#Search for the optimal production level for the maximum revenue op=0
for(k in 1:100){
  if(rev(x)[k] > 28.7598){
    op=x[k]} 
}
op

crrev <- function(x) {(10000/(x+1)^2)*(cos(0.5*(x-8.5)))- 
    0.5*x*(10000/(x+1)^2)*(sin(0.5*(x-8.5)))- 
    2*x*(10000/(x+1)^3)*(cos(0.5*(x-8.5))) } 
uniroot(crrev,c(7.5,11))

x <- seq(7.5,11.0,length=100)
mrev <- function(x) {10^(-3)*30*x*exp(0.5*(x-10.0))*(10000/(x+1)^2)*(
  cos(0.5*(x-8.5)))}
plot(x,mrev(x),type="l", main="Market-Modified Oil Revenue vs. Oil
     Production for a Country", xlab="Oil Production [Mbbl/Day]",
     ylab="Billion USD" )
grid(nx = NULL, ny = NULL)

#Search for the maximum revenue
max(mrev(x))

#Search for the optimal production level for the maximum revenue op=0
for(k in 1:100){
  if(mrev(x)[k] > 18.18664){
    op=x[k]} 
}
op