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
# Chapter 4: Principles of Mathematical Modeling
#
################################################################


### Moon's Surface Temperature
{r}
#NASA Diviner Data Source: 
#http://pds-geosciences.wustl.edu/lro/lro-l-dlre-4-rdr-v1/lrodlr_1001/data/gcp/

#setwd("~/sshen/mathmodel") 
d19 <- read.table("~/Desktop/RMathModel/data/moon_temp_snapshot_half_deg.txt",
                  header=FALSE)
dim(d19)
#259200 grid points at 0.5 lat-lon resolution 
#259200=720*360, starting from (-179.75, -89.75) going north
#then back to south pole then going north
#until the end (179.75, 89.75)

m19 <- matrix(d19[,3], nrow=360) #data matrix for map plotting
dim(m19)

library(maps) 
Lat1 <- seq(-89.75, by=0.5, len=360) 
Lon1 <- seq(-179.75, by=0.5, len=720)
mapmat <- t(m19)

#plot.new()
#png(filename = paste("Moon Surface Temperature, Snapshot=", 19,".png"), 
#    width=800, height=400)

int <- seq(0,400,length.out=40)
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                 "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
filled.contour(Lon1, Lat1, mapmat, color.palette=jet.colors, levels=int, 
               plot.title=
                 title("Moon Surface Temperature Observed by\nNASA Diviner, Snapshot 19",
                       xlab="Longitude", ylab="Latitude"), 
               plot.axes={axis(1); axis(2);grid()}, 
               key.title=title(main="K"))
#dev.off()

# Plot the equator temperature for a snapshot
#plot.new()
#png(filename=paste("Moon's Equatorial Temperature at Snapshot", 19,".png")
#    ,width=600, height=400)
plot(Lon1,m19[180,],type="l", col="red",lwd=2, 
     xlab="Longitude", ylab="Temperature [K]", 
     main="Moon's Equatorial Temperature at Snapshot 19")
grid(nx = NULL, ny = NULL)
text(-100,250,"Nighttime",cex=2)
text(80,250,"Daytime",cex=2, col="orange")
#dev.off()

# Plot the noon time meridional temperature for a snapshot
#plot.new()
#png(filename=paste("Moon's Noon Time Meridional Temperature at Snapshot", 
#                   19,".png"), width=600, height=400)
plot(Lat1,m19[,540],type="l", col="red",lwd=2,
     xlab="Latitude", ylab="Temperature [K]",
     main="Moon's Noon Time Meridional Temperature at Snapshot 19")
grid(nx = NULL, ny = NULL)
#dev.off()

#Compute the temperature of the bright side average
bt <- d19[129601:259200,] 
aw <- cos(bt[,2]*pi/180) 
wbt <- bt[,3]*aw 
bta <- sum(wbt)/sum(aw) 
bta #Kelvin

#Compute the temperature of the dark side average
dt <- d19[0:12960,] 
aw <- cos(dt[,2]*pi/180) 
wdt <- dt[,3]*aw 
dta <- sum(wdt)/sum(aw) 
dta #Kelvin



### EBM Prediction for the Moon Surface Temperature

#Equator noon
lat <- 0*pi/180
sigma <- 5.670367*10^(-8)
alpha <- 0.12
S <- 1368
ep <- 0.98
k <- 7.4*10^(-4)
h <- 0.4
T0 <- 260
fEBM <- function(T){(1-alpha)*S*cos(lat) - (ep*sigma*T^4 + k*(T-T0)/h)} 
#Numerically solve this EBM (Energy Balanced Model) : fEBM = 0
uniroot(fEBM,c(100,420))



### EBM for a Uniform Earth with Nonlinear Albedo Feedback

#Figure 4.7
T <- seq(200,350, by=0.1)
y1 <- 0.5 - 0.2 * tanh ((T-265)/10)
plot(T, y1, 
     xlim = c(200, 350), ylim=c(0,1),
     xaxp=c(200, 350, 15), yaxp=c(0, 1, 10),
     main="Albedo as a Function of Surface Temperature", 
     ylab=expression(paste("Albedo ", alpha)),
     xlab="Earth's Surface Temperature [K]",
     type = "l", 
     lwd=2,
     col="black")
grid(nx = NULL, ny = NULL)
text(222,0.75,"Ice-covered Earth", col="blue",cex=1.2) 
text(330,0.35,"Ice-free Earth", col="red",cex=1.2)

#Figure 4.8
S <- 1365
ep <- 0.6
sg <- 5.670373*10^(-8)
T <- seq(200,350, by=0.1)
y1 <- (1-(0.5 - 0.2 * tanh ((T-265)/10)))*(S/4)
y2 <- ep*sg*T^4
plot(T, y1, 
     xlim=c(200, 350), ylim=c(0,300), 
     xaxp=c(200, 350, 15), yaxp=c(0, 300, 10), 
     main="Simple Nonlinear EBM with Albedo Feedback", 
     ylab= bquote("Energy [W/" ~m^2~"]"),
     xlab="Temperature [K]",
     type = "l",col="red")
grid(nx = NULL, ny = NULL)
lines(T, y2, col="blue")
lines(T, y2-y1, col="purple")
y3 <- 0.0*T
lines(T, y3, col="green")
text(310,220, "Incoming Energy", col="red",cex=1.2) 
text(290,280, "Outgoing Energy", col="blue",cex=1.2) 
text(310,60, "Outgoing Energy - Incoming Energy", 
     col="purple",cex=1.2)

# Precise values of these three numerical solutions
S <- 1365
ep <- 0.6
sg <- 5.670373*10^(-8)
f <- function(T){return(ep*sg*T^4 -
                          (1-(0.5 - 0.2 * tanh ((T-265)/10)))*(S/4))}
uniroot(f,c(220,240))
uniroot(f,c(260,275))
uniroot(f,c(275,295))

