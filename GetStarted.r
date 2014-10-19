### Ice Analysis
### October 7th, 2014
library(EBImage)
pics<-dir(pattern=".JPG")  ## Load a directory of images
i=1
while (i <= length(pics)) { 
  p<-readImage(pics[i]) ## Read a picture 
  #display(p) ## Display
  fp<-flip(p) ### Rotate
  #display(fp)  ## Display again
  colorMode(fp) = Grayscale ## switch to grayscale
  #display(fp) ## chec via display
  ffp<-getFrame(fp,1, type='total') ### isolate frame #1
  goof<-ffp[1:2560, 830:1300] ### crop to just the ice
  hcp<-goof*2
  display(hcp) ### display it
  writeImage(hcp, paste(substr(paste(pics[i]),1,8), "cr.jpeg", sep=""), quality=85)
  i=i+1
}


### LOADS the cropped images
library(EBImage)
library(ggplot2)
cropped<-dir(pattern="cr") ## Just the cropped images
ii=1
outputb<-dim(0)
vars<-dim(0)
sds<-dim(0)
means<-dim(0)
names<-dim(0)
while (ii <= length(cropped)) { 
  c1<-readImage(cropped[ii])
  #display(c1, method="raster")
  #str(c1)
  x<-data.frame(as.numeric(c1))
  #m <- ggplot(x, aes(x=as.numeric.c1.))
  #m + geom_histogram(binwidth = .02)
  vars[ii]<-var(x$as.numeric.c1.)
  sds[ii]<-sd(x$as.numeric.c1.)
  means[ii]<-mean(x$as.numeric.c1.)
  names[ii]<-paste(cropped[ii])
  outputb[ii]<-(sum((as.numeric(c1)>.99)*1))/length(as.numeric(c1))
  ii=ii+1
}
crsums<-data.frame(names=names, percwhite=outputb, means=means, sds=sds, vars=vars, sample=seq(1:50))


## READ the exif data
ii=1
finfo<-file.info(pics)
final<-cbind(crsums,finfo)
head(final)


## DOES SOME PLOTTING
library(ggplot2)
ggplot(data=final, aes(x=mtime, y=percwhite)) + geom_line() + ylim(0,1) + geom_smooth()
ggplot(data=final, aes(x=mtime, y=means)) + geom_line() + ylim(0,1) + geom_smooth()
ggplot(data=final, aes(x=mtime, y=sds)) + geom_line() + ylim(0,1) + geom_smooth()
ggplot(data=final, aes(x=mtime, y=vars)) + geom_line() + ylim(0,1) + geom_smooth()


cropped[ii]
head(crsums)


p1<-readImage(pics[5]) ## Read a picture 
display(p1) ## Display
fp<-flip(p1) ### Rotate
display(fp)  ## Display again
colorMode(fp) = Grayscale ## switch to grayscale
display(fp) ## chec via display
ffp<-getFrame(fp,1, type='total') ### isolate frame #1
goof<-ffp[1:2560, 830:1300] ### crop to just the ice
display(goof) ### display it

