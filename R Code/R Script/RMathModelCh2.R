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
# Chapter 2: Basics of R Programming
#
################################################################

### R as a Smart Calculator
1 + 4 #The text behind the symbol # is a comment in R 
2 + pi/4-0.8 
#pi is circumference ratio which is approximately 3.1415926 
x <- 1 
# <- is the assignment symbol: it assigns 1 to x.
y <- 2
z <- 4
t <- 2*x^y-z #This statement is equivalent to 2*1^2-4 = -2
t

u = 2 
v = 3
# Symbols "=" and "<-" mean the same in most cases
u + v
sin(u*v) # u*v = 6 is considered a radian value

### Write a function in R
square <- function(x) x*x 
# This statement creates function where it is a block of code
# that runs when it is called.
# It may pass in data which are called parameters 
square(4)
fctn <- function(x,y,z) {x+y-z/2} 
fctn(1,2,3)


### Plot with R
plot(sin, -pi, 2*pi) #Plot the curve of y = sin(x) from -pi to 2*pi 
square <- function(x) x*x #Define a function
plot(square, -3, 2)# Plot the defined function

# Plot a 3D surface using the following code
x <- y <- seq(-1, 1, length=100)
Z <- outer(x, y, function(x, y){1-x^2-y^2})
# outer(x,y, function) gives the z values defined on x, y grid
persp(x = x, y = y, z = Z, theta = 310)
# yields a 3D surface with perspective angle 310 deg


### Symbolic Calculation for Calculus by R
D(expression(x^2,'x'), 'x') # Take derivative of x^2 which is 2x

fx <- expression(x^2,'x') 
#define a symbolic function with variable x 
D(fx,'x') #differentiate the function with respect to x

fx <- expression(x^2*sin(x),'x')
#Change the expression and use the same derivative command
D(fx,'x')

fxyz <- expression(x^2+y^2+z^2, 'x','y','z')
#define a function of two or more variables
fxyz #This gives the expression of the function in x, y and z 
D(fxyz,'x') #gives the partial derivative with respect to x: 2 * x 
D(fxyz,'y') #gives the partial derivative with respect to y: 2 * y 

square <- function(x) x^2
integrate(square, 0, 1) 
#Numerically integrating x^2 from 0 to 1 
# gives 0.3333333 with absolute error < 3.7e-15

integrate(cos, 0, pi/2) 
#Integrates cos(x) from 0 to pi/2 
# gives 1 with absolute error < 1.1e-14


### Vectors and Matrices
c(1, 6, 3, pi, -3) #Enter data inside c() to create a 5x1 column vector 

seq(2, 6) #Generate a sequence from 2 to 6

seq(1, 10, 2) # Generate a sequence from 1 to 10 with an increment of 2

x <- c(1, -1, 1, -1)
x + 1 #1 is added to each element of x 
2*x #2 multiplies each element of x
x/2 #Each element of x is divided by 2

y <- seq(1, 4)
x*y # * multiples each pair of elements

t(x) # Transpose of x matrix

t(x)%*%y #Matrix multiplication: 1x4 matrix times a 4x1 matrix 
#equivalent to a dot product

x%*%t(y) #4x1 matrix times a 1x4 matrix yields a 4x4 matrix

mx <- matrix(x, nrow=2)
#Convert a vector y into a matrix of 2 rows. 
mx
#or simply
my <- matrix(y,2)
my #The matrix elements go by column, first column, second, etc
dim(my) #Find dimensions of a matrix
as.vector(my) #Convert a matrix to a vector, also via columns 
mx*my #Multiplication between each pair of elements
mx/my #Division between each pair of elements
mx-2*my
mx%*%my
det(my) #Determinant of a square matrix
myinv <- solve(my) #Find inverse of a matrix 
myinv
myinv%*%my #verifies the inverse of a matrix and results in identity matrix
diag(my) #outputs the diagonal vector of a matrix

myeig <- eigen(my)
myeig #yields the two eigenvalues and eigenvectors
myeig$values #outputs only eigenvalues 
myeig$vectors #outputs only eigenvectors

mysvd <- svd(my) #SVD decomposition of a matrix M = UDV'
#SVD can be done for any m-by-n rectangular matrix 
mysvd 
#outputs d, U, and V where d is the SVD eigenvalues
# U is the spatial eigenvectors, and V is the temporal eigenvectors

mysvd$d #outputs d only, as a vector 
U <- mysvd$u 
U #outputs the U matrix 
D <- diag(mysvd$d) 
#generates the diagonal D matrix

V <- mysvd$v 
V #outputs the V matrix
U%*%D%*%t(V) #recovers the original matrix 'my'

b <- c(1, 3)
ysol <- solve(my, b) 
#solve linear equations my %*% x = b 
ysol 

my%*%ysol #verifies the solution, equal to b


### Statistics
x <- rnorm(8) 
#generates 8 random numbers in (-infty, infty)
#with normal distribution with default mean = 0, and default sd = 1
#If you want to set a specific mean and sd, you need to place in the
#function call
x

mean(x) #mean

var(x) #variance

sd(x) #standard deviation

median(x) #median

quantile(x) #0%         25%         50%         75%        100% 

range(x) #yields the min and max of x

max(x) #maximum value of x

boxplot(x) #yields the box plot of x

w <- rnorm(1000) 
#generate 1000 random numbers in N(0,1) distribution

z <- rnorm(10000, mean = 10, sd = 5) 
#generate 100 random numbers following N(10,5^2)
#mean = 10, standard deviation = 5

summary(rnorm(12)) #statistical summary of the data sequence

hist(w) #plot the histogram of 1000 random numbers

#Find the current directory
getwd()

##Change directory 
#setwd("/Users/sshen/Desktop/MyDocs/teach") #for Mac 
#setwd("D:/My Documents/Desktop/teach") #for PC


### R Tutorials

setwd("~/Desktop/RMathModel/data")

##Commands to input data files:

mydata <- read.csv("mydata.csv") 
# read csv file named "mydata.csv"

mydata <- read.table("mydata.txt") 
# read text file named "mydata.txt"

library(gdata) 
# load the gdata package 

# Install packages by using install.packages
# install.packages("readxl")
library("readxl")

mydata <- read_excel("mydata.xlsx") 
# read an excel file

library(foreign) 
# load the foreign package 

#mydata <- read.mtp("mydata.mtp") 
# read from .mtp file


#mydata <- read.spss("myfile", to.data.frame=TRUE)
#read a spss file

ff <- tempfile()
cat(file = ff, "123456", "987654", sep = "\n") 
read.fortran(ff, c("F2.1","F2.0","I2"))
# read a fotran file

library(ncdf4)
# load the ncdf4 package 

ncin <- ncdf4::nc_open("air.mon.mean.nc") 
# open a NetCDF file

lon <- ncvar_get(ncin, "lon") 
#read data "lon" from a netCDF file into R

library("rjson")
# load the rjson package 

jd <- fromJSON(file = "argo4903285_59.json")
# read data from a JSON file argo4903285_59.json

library(ncdf4) 
#Suppose the following error message pops up when you load ncdf4 package
#Error in library(ncdf4) : there is no package called ncdf4 

#Then you can install the R package by
#install.packages("ncdf4")

#R packages: animation, chron, e1071, fields, ggplot2, lattice,
#latticeExtra, maps, mapdata, mapproj, matrixStats, ncdf4,
#NLRoot, RColorBrewer, rgdal, rasterVis, raster, rjson, sp, TTR

#The zipped data for this book can be downloaded from: 
#climatemathematics.sdsu.edu/data.zip

#To load a single package, such as "animation", you can do
library(animation)

#You can also load all these packages at once using pacman package
#install.packages("pacman")
library(pacman)
pacman::p_load(animation, chron, e1071, fields, ggplot2, lattice,
               latticeExtra, maps, mapdata, mapproj, matrixStats, ncdf4,
               NLRoot, RColorBrewer, rasterVis, raster, rjson, sp, TTR)
