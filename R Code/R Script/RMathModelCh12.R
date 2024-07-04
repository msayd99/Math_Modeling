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
# Chapter 12: Concepts of Machine Learning
#
################################################################

### K-means Clustering

# K-Means Cluster Analysis for a 2D Random Point Set 
mydata <- matrix(runif(40),ncol=2)
fit <- kmeans(mydata, 5) # 5 cluster solution

# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mycluster <- data.frame(mydata, fit$cluster) 
library(animation)
kmeans.ani(mycluster, centers=5, pch=1:5, col=1:5)