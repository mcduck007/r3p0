#set working directory
setwd('~/Code/01/')

# load the data into a dataframe
# replace the file name
wl=read.csv('adhocData635376701169286071.csv',header=F)

#shows how the data is structured
str(wl)

#extracting levels and more...
wl.build=wl$V1
wl.latency=wl$V3

wl.colors=seq(1:(length(levels(wl$V1))))
wl.levels=levels(wl$V1)


#plot build vs latency
pairs(wl.build~wl.latency)

#show descriptive stats for every build
by(wl$V3,wl$V1,summary)


#start plotting routine 1
library(vioplot)

lim_y=c(0,5e5)
lim_x=c(0,length(wl.levels)+1)

x=-1
y=-1
plot(x,y,xlim=lim_x,ylim=lim_y,xlab='Build',ylab='Time(ms)')

wl.tmp=1
while(wl.tmp<=length(wl.levels)){
  vioplot(wl$V3[wl$V1==wl.levels[wl.tmp]],
          add=TRUE,
          col=wl.tmp,
          horizontal=FALSE,
          at=wl.tmp)
  wl.tmp=wl.tmp+1
}
legend('topleft',wl.levels,horiz=FALSE,fill=wl.colors)
#end plotting routine 1



#start scaled plotting routine here

lim_y=c(0,1500)
lim_x=c(0,length(wl.levels)+1)
library(vioplot)
x=-1
y=-1
plot(x,y,ylim=lim_y,xlim=lim_x,xlab='Build',ylab='Time(ms)')

wl.tmp=1
while(wl.tmp<=length(wl.levels)){
  vioplot(wl$V3[wl$V1==wl.levels[wl.tmp]],
          add=TRUE,
          col=wl.tmp,
          horizontal=FALSE,
          at=wl.tmp)
  wl.tmp=wl.tmp+1
}
legend('topleft',wl.levels,horiz=FALSE,fill=wl.colors)
#end scaled plotting routine

#find iqrs and quantiles for every level
wl.iqr= 
  wl.qnt25= 
  wl.qnt75= 
  wl.median= 
  wl.mean= 
  wl.length= 
  wl.low_outl=
  wl.high_outl=
  wl.low_pct=
  wl.high_pct=rep(0,length(wl.levels))


wl.tmp=1
while(wl.tmp<=length(wl.levels)){
  wl.iqr[wl.tmp]=IQR(wl$V3[wl$V1==wl.levels[wl.tmp]])
  wl.median[wl.tmp]=median(wl$V3[wl$V1==wl.levels[wl.tmp]])
  wl.mean[wl.tmp]=mean(wl$V3[wl$V1==wl.levels[wl.tmp]])
  wl.length[wl.tmp]=length(wl$V3[wl$V1==wl.levels[wl.tmp]])
  wl.qnt25[wl.tmp]=quantile(wl$V3[wl$V1==wl.levels[wl.tmp]],0.25,names=FALSE)
  wl.qnt75[wl.tmp]=quantile(wl$V3[wl$V1==wl.levels[wl.tmp]],0.75,names=FALSE)
  wl.low_outl[wl.tmp]=-0.75*wl.iqr[wl.tmp]+wl.qnt25[wl.tmp]
  wl.high_outl[wl.tmp]=0.75*wl.iqr[wl.tmp]+wl.qnt75[wl.tmp]
  
  wl.high_pct[wl.tmp]=
    sum((wl$V3[wl$V1==wl.levels[wl.tmp]])>=wl.high_outl[wl.tmp])/wl.length[wl.tmp]
  
  wl.low_pct[wl.tmp]=
    sum((wl$V3[wl$V1==wl.levels[wl.tmp]])<=wl.low_outl[wl.tmp])/wl.length[wl.tmp]
  
  wl.tmp=wl.tmp+1
}


