# Al Pakrosnis
# Homework 6
# Prof. Dale Embers
# STAT385 Sp25

setwd("~/Desktop/stat385/")

# Q1
df <- read.table("homework6/Hemophilia-dat.txt", header = FALSE)
summary(df)
head(df)

colnames(df) <- c("group","AHF activity","AHF-like antigen")

set.seed(2024325)

train <- sample(1:dim(df)[1],60)

# Q2
