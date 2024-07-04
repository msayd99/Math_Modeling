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
# Chapter 5: Mathematical Modeling By Linear Algebra
#
################################################################

### Kirchhoff's Laws and Solution of an Electric Circuit
{r}
#Step 4: Solution to the mathematical model equations
a <- matrix(c(1,8,0,-1,0,6,1,-3,3), 
            nrow=3, ncol=3) 
b <- matrix(c(0,10,15), 
            nrow=3, ncol=1)
solve(a, b)

#Step 5: Interpretation of the Modeling Results
c <- solve(a, b)
a %*% c #verify the solution: a*c = b 



### Leontief Production Model: A Balance of Output and Input

B <- matrix(c(1-0.3, -0.2, -0.3,
              -0.2, 1-0.2, -0.2,
              -0.2, -0.1, 1-0.3), nrow=3, ncol=3)
D <- matrix(c(3000, 500, 1500), nrow=3, ncol=1) 
solve(B, D)

I <- diag(3) #3-dim identity matrix
A <- matrix(c(0.4102, 0.0301, 0.0257,
              0.0624, 0.3783, 0.1050,
              0.1236, 0.1588, 0.1919),
            byrow = TRUE, nrow = 3)
D <- c(39.24,60.02,130.65)
solve(I-A, D)


### The Fundamental Idea of SVD: Space-Time-Energy Separation
#### SVD demo for generated data: A = UDV'

#Demonstrate SVD for a simple 2X3 matrix 
a1 <- matrix(c(1,1,0,
               -1,-1,0),
             nrow=2) 
a1

svda1 <- svd(a1) 
U <- svda1$u 
DE <- svda1$d 
V <- svda1$v
U
V
DE

D <- diag(DE) #forms the SVD diagonal matrix 

#Verification of SVD: A = UDV'
#round() removes decimal places
#This yields the original data matrix a1
round(U%*%D%*%t(V)) 

#Graphically show the U column vectors, aka EOFs
#Figure 5.2
par(mar=c(1,0,0.5,0.5))
#plot.new()
#par(mfrow = c(2,2), byrow = TRUE) 
layout(matrix(c(1,2,3,4), nrow=2, byrow = TRUE) )
par(mgp=c(2,1,0), mar=c(4,4,3,1)) 

plot(NULL,
     xlim = c(0.5, 2.5),
     ylim = c(-2,2),
     xaxt = "n", yaxt = "n",
     xlab="", ylab="",
     bty = 'n',
     main="EOF1 = U[,1]")
points(1, 0, pch = 16, 
       cex=5*0.707, col = 'blue')
text(1, -1.2, 'Spatial1', cex =1.3)
text(1, 1.2, '-0.707', cex =1.3)
points(2,0, pch = 16, 
       cex=5*0.707, col = 'blue')
text(2, -1.2, 'Spatial2', cex =1.3)
text(2, 1.2, '-0.707', cex =1.3)

par(mgp=c(2,1,0),mar=c(4,4,3,1))
plot(1:3, V[,1],
     type = 'o',
     xlim = c(1, 3),
     ylim = c(-1,1),
     xlab="Time", ylab="Scale",
     bty = 'n',
     main="PC1 = V[,1]")
grid(nx = NULL, ny = NULL)

par(mgp=c(2,1,0),mar=c(4,4,3,1))
plot(NULL,
     xlim = c(0.5, 2.5),
     ylim = c(-2,2),
     xaxt = "n", yaxt = "n",
     xlab="", ylab="",
     bty = 'n',
     main="EOF2 = U[,2]")
points(1, 0, pch = 16, 
       cex=5*0.707, col = 'blue')
text(1, -1.2, 'Spatial1', cex =1.3)
text(1, 1.2, '-0.707', cex =1.3)
points(2,0, pch = 16, 
       cex=5*0.707, col = 'red')
text(2, -1.2, 'Spatial2', cex =1.3)
text(2, 1.2, '0.707', cex =1.3)

par(mgp=c(2,1,0),mar=c(4,4,3,1))
plot(1:3, V[,2],
     type = 'o',
     xlim = c(1, 3),
     ylim = c(-1,1),
     xlab="Time", ylab="Scale",
     bty = 'n',
     main="PC2 = V[,2]")
grid(nx = NULL, ny = NULL)


### Example: 5.2

#SVD demo for generated data

#Generate random data on a 10-by-15 grid with 20 time points 
#SVD for 2D spatial dimension and 1D time rm(list=ls())
#remove the R console history
dat <- matrix(rnorm(10*15*20),ncol=20)
x <- 1:10
y <- 1:15
udv <- svd(dat) 
U <- udv$u 
D <- udv$d 
V <- udv$v

dim(U)
dim(V)
length(D)
#Plot spatial pattern for EOF1 
umat <- matrix(U[,1],nrow=15) 
dim(umat)

#Figure 5.3
plot.new() 
#start a new figure from blank 
par(mar=c(4.5,4.5,2,1))
filled.contour(x, y, t(umat),key.title = title(main = "Scale"),
               plot.axes = {axis(1,seq(0,10, by = 2), cex.axis=1.3) 
                 axis(2,seq(2, 15, by = 3), cex.axis=1.3)},
               plot.title = title(main = "Spatial Pattern: EOF1", 
                                  xlab="Spatial x Position: 1 to 10",
                                  ylab="Spatial y Position: 1 to 15", 
                                  cex.lab=1.5),
               color.palette = colorRampPalette(
                 c("red", "white", "blue")))
#Plot time pattern PC1
par(mfrow=c(1,1))
par(mar=c(4,4,2,1))
plot(1:20, V[,1],type="o",col="red",lwd=2,
     main="Temporal Pattern: Time Series",xlab="Time", 
     ylab="PC1 Values [Dimensionless]",cex.lab=1.3, cex.axis=1.3)
grid(nx = NULL, ny = NULL)


### SVD Analysis for El Niño Southern Oscillation Data

#setwd("~/sshen/mathmodel")
Pta <- read.table("~/Desktop/RMathModel/data/PSTANDtahiti.txt", header=F)
# Remove the first column that is the year
ptamon <- Pta[,seq(2,13)]
#Convert the matrix into a vector according to months: Jan 1951, Feb 1951,
# ..., Dec 2015
ptamonv <- c(t(ptamon))
xtime <- seq(1951, 2016-1/12, 1/12)

# Figure 5.4
# Plot the Tahiti standardized SLP anomalies
plot(xtime, ptamonv,type="l",xlab="Year",ylab="Pressure",
     main="Standardized Tahiti SLP Anomalies", col="red",
     xlim=range(xtime), ylim=range(ptamonv))
grid(nx = NULL, ny = NULL)
# Do the same for Darwin SLP
Pda <- read.table("~/Desktop/RMathModel/data/PSTANDdarwin.txt", header=F)
pdamon <- Pda[,seq(2,13)]
pdamonv <- c(t(pdamon))
plot(xtime, pdamonv,type="l",xlab="Year",ylab="Pressure",
     main="Standardized Darwin SLP Anomalies", col="blue",
     xlim=range(xtime), ylim=range(pdamonv))
grid(nx = NULL, ny = NULL)
#Plot the SOI index
plot(xtime, ptamonv-pdamonv,type="l",xlab="Year",
     ylab="SOI Index", col="black",xlim=range(xtime), ylim=c(-4,4), lwd=1)
grid(nx = NULL, ny = NULL)
#Add ticks on top edge of the plot box
axis(3, at=seq(1951,2015,4), labels=seq(1951,2015,4))
# Add ticks on the right edge of the plot box
axis(4, at=seq(-4,4,2), labels=seq(-4,4,2))
# If put a line on a plot, use the command below
lines(xtime,ptamonv-pdamonv,col="black", lwd=1)

cnegsoi <- -cumsum(ptamonv-pdamonv)
plot(xtime, cnegsoi,type="l",xlab="Year",ylab="Negative CSOI Index",
     col="black",xlim=range(xtime), ylim=range(cnegsoi), lwd=1)
grid(nx = NULL, ny = NULL)
#Time-space data format
ptada <- cbind(ptamonv,pdamonv)
#Space-time data format
ptada <- t(ptada)
dim(ptada)
svdptd <- svd(ptada)
# Verify that recontd = ptada
recontd <- svdptd$u%*%diag(svdptd$d[1:2])%*%t(svdptd$v)
recontd
U <- svdptd$u
U

svdptd$d
D <- diag(svdptd$d)
D

V <- svdptd$v
V


### Figure 5.5

#Display the two ENSO modes on a world map
library(maps)
library(mapdata)

plot.new()
par(mfrow=c(2,1))

par(mar=c(0,0,0,0)) #Zero space between (a) and (b)
map(database="world2Hires",ylim=c(-70,70), mar = c(0,0,0,0))
grid(nx=12,ny=6)
points(231, -18,pch=16,cex=2, col="red")
text(231, -30, "Tahiti 0.61", col="red")
points(131, -12,pch=16,cex=2.6, col="blue")
text(131, -24, "Darwin -0.79", col="blue")
axis(2, at=seq(-70,70,20),
     col.axis="black", tck = -0.05, las=2, line=-0.9,lwd=0)
axis(1, at=seq(0,360,60),
     col.axis="black",tck = -0.05, las=1, line=-0.9,lwd=0)
text(180,30, "El Niño Southern Oscillation Mode 1",col="purple",cex=1.3)
text(10,-60,"(a)", cex=1.4)
box()

par(mar=c(0,0,0,0)) #Plot mode 2
map(database="world2Hires",ylim=c(-70,70), mar = c(0,0,0,0))
grid(nx=12,ny=6)
points(231, -18,pch=16,cex=2.6, col="red")
text(231, -30, "Tahiti 0.79", col="red")
points(131, -12,pch=16,cex=2, col="red")
text(131, -24, "Darwin 0.61", col="red")
text(180,30, "El Niño Southern Oscillation Mode 2",col="purple",cex=1.3)
axis(2, at=seq(-70,70,20),
     col.axis="black", tck = -0.05, las=2, line=-0.9,lwd=0)
axis(1, at=seq(0,360,60),
     col.axis="black",tck = -0.05, las=1, line=-0.9,lwd=0)
text(10,-60,"(b)", cex=1.4)
box()



### Figure 5.6

#Plot WSOI1
xtime <- seq(1951, 2016-1/12, 1/12)
wsoi1 <- D[1,1]*t(V)[1,]
plot(xtime, wsoi1,type="l",xlab="Year",ylab="Weighted SOI 1",
     col="black",xlim=range(xtime), ylim=range(wsoi1), lwd=1)
axis(3, at=seq(1951,2015,4), labels=seq(1951,2015,4))
grid(nx = NULL, ny = NULL)
#Plot WSOI2
wsoi2 <- D[2,2]*t(V)[2,]
plot(xtime, wsoi2,type="l",xlab="Year",ylab="Weighted SOI 2",
     col="black",xlim=range(xtime), ylim=c(-2,2), lwd=1)
axis(3, at=seq(1951,2015,4), labels=seq(1951,2015,4))
grid(nx = NULL, ny = NULL)
#Plot Cumulative WSOI1
cwsoi1 <- cumsum(wsoi1)
plot(xtime, cwsoi1,type="l",xlab="Year",ylab="Cumulative Weighted SOI 1",
     col="black",xlim=range(xtime), ylim=range(cwsoi1), lwd=1)
axis(3, at=seq(1951,2015,4), labels=seq(1951,2015,4))
grid(nx = NULL, ny = NULL)
#Plot Cumulative WSOI2
cwsoi2 <- cumsum(wsoi2)
plot(xtime, cwsoi2,type="l",xlab="Year",ylab="Cumulative Weighted SOI 2",
     col="black",xlim=range(xtime), ylim=range(cwsoi2), lwd=1)
axis(3, at=seq(1951,2015,4), labels=seq(1951,2015,4))
grid(nx = NULL, ny = NULL)

