#### libraries
library(unmarked)
library(boot)
library(tidyverse)
library(reshape2)
#####################
# set working directory
setwd("/Users/Kullu/Desktop/Chagsaa_reanalysis")

#reading data
dat.det<-read.csv("data_ibex_argali_det.csv")
dat.cov<-read.csv("data_ibex_argali_cov.csv")

#creating appropriate subsets
dat.ibex<-subset(dat.det, select = c("Ibex1","Ibex2","Ibex3","Ibex4","Ibex5","Ibex6"))
dat.argali<-subset(dat.det, select = c("Argali1","Argali2","Argali3","Argali4","Argali5","Argali6"))

#Running occupancy models
arg.occu.dat<-unmarkedFrameOccu(y = dat.argali, 
                                siteCovs = dat.cov)
mod.argali<-occu(~snow ~rugg + lives, arg.occu.dat)

summary(mod.argali)
backTransform(mod.argali, 'det')
coef(mod.argali, type = 'state')[2]

#ibex
ibx.occu.dat<-unmarkedFrameOccu(y = dat.ibex, 
                                siteCovs =dat.cov)
mod.ibex<-occu(~1 ~rugg, ibx.occu.dat)

summary(mod.ibex)
backTransform(mod.ibex, 'det')
coef(mod.ibex, type = 'state')[2]

#Conducting the bootstrap for Argali 
len<-1000
output.rugg<-rep(NA, len)
output.lives<-rep(NA, len)
for(i in 1:len){
  counter<-sample(1:length(dat.argali[,1]), length(dat.argali[,1]), replace = F)
  mod.argali<-occu(~snow ~rugg[counter] + lives[counter], arg.occu.dat)
  output.rugg[i]<-coef(mod.argali, type = 'state')[2]
  output.lives[i]<-coef(mod.argali, type = 'state')[3]
}

length(which(output.rugg < -3.1))/1000 ## proportion of times we got higher than the expected correlation
length(which(output.lives < -2.15))/1000

#Conducting the bootstrap for ibex 
len<-1000
output.rugg.ibex<-rep(NA, len)
for(i in 1:len){
  counter<-sample(1:length(dat.ibex[,1]), length(dat.ibex[,1]), replace = F)
  mod.ibex<-occu(~1 ~rugg[counter], ibx.occu.dat)
  output.rugg.ibex[i]<-coef(mod.ibex, type = 'state')[2]
}

length(which(output.rugg.ibex > 3.21))/1000 ## proportion of times we got higher than the expected correlation
