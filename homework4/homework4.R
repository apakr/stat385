# Al Pakrosnis
# Prof. Dale Embers
# STAT 385
# Homework 4

setwd("~/Desktop/stat385/homework4/")
getwd()

chromoX <- read.table(file="ChromoXmicroarray.txt",header=TRUE)

head(chromoX)

BLClabels <- c(rep(1,13),rep(2,14),rep(3,20))
BLClabels

# Q1. Setup
set.seed(2024)
train.set<-sample(c(1:47),27)

# Q2. 