## Q1: Read image into R, and covert it into numeric numbers

setwd("~/Desktop/stat385/lab9/")
library(png)
rgb.array <- readPNG("Lenna.png")
red.matrix<-rgb.array[,,1]
green.matrix<-rgb.array[,,2]
blue.matrix<-rgb.array[,,3]

## Q2: Use red color array to perform PCA, and find the variation explained by each PC

red.pca<-prcomp(red.matrix,center=FALSE)
plot(red.pca,type="l")
totalvariance<-sum((red.pca$sdev)^2)
varianceratio<-(red.pca$sdev)^2/totalvariance
plot(varianceratio[1:20],xlab="Component Number")
plot(cumsum(varianceratio[1:20]),xlab="Component Number")
# Q: How many components should we keep to maintain 80% of the variation?
# A: 1

## Q3: Visualize the original image and compressed image for the red component

first20PC<-red.pca$rotation[,1:20]
PC20score<-red.pca$x[,1:20]
red.img.proj<-PC20score%*%t(first20PC)
par(mfrow=c(1,2))
im=matrix(red.matrix, nrow = 512, ncol = 512)
mat1<-apply(im,2,rev)
image(t(mat1),col=rgb((0:512)/512,0,0))
construtim<-matrix(red.img.proj, nrow = 512, ncol = 512)
mat2<-apply(construtim,2,rev)
image(t(mat2),col=rgb((0:512)/512,0,0))

## Q4: Repeat Q3 for the green and blue components

green.pca<-prcomp(green.matrix,center=FALSE)
first20PCGreen<-green.pca$rotation[,1:20]
PC20scoreGreen<-green.pca$x[,1:20]
green.img.proj<-PC20scoreGreen%*%t(first20PCGreen)
par(mfrow=c(1,2))
im=matrix(green.matrix, nrow = 512, ncol = 512)
mat3<-apply(im,2,rev)
image(t(mat3),col=rgb(0,(0:512)/512,0))
construtim<-matrix(green.img.proj, nrow = 512, ncol = 512)
mat4<-apply(construtim,2,rev)
image(t(mat4),col=rgb(0,(0:512)/512,0))

blue.pca<-prcomp(blue.matrix,center=FALSE)
first20PCBlue<-blue.pca$rotation[,1:20]
PC20scoreBlue<-blue.pca$x[,1:20]
blue.img.proj<-PC20scoreBlue%*%t(first20PCBlue)
par(mfrow=c(1,2))
im=matrix(blue.matrix, nrow = 512, ncol = 512)
mat5<-apply(im,2,rev)
image(t(mat5),col=rgb(0,0,(0:512)/512))
construtim<-matrix(blue.img.proj, nrow = 512, ncol = 512)
mat6<-apply(construtim,2,rev)
image(t(mat6),col=rgb(0,0,(0:512)/512))

## Q5: Combine the compressed RGB images together and save the reconstruct the image

recon.rgb.array<-rgb.array
recon.rgb.array[,,1]<-red.img.proj
recon.rgb.array[,,2]<-green.img.proj
recon.rgb.array[,,3]<-blue.img.proj
filename<-"CompressedLenna20.png"
writePNG(recon.rgb.array, filename)
library("grid")
grid.raster(readPNG(filename, native=TRUE))



