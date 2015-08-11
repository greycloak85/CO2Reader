#setwd("C:\\Users\\notebook\\Dropbox\\Experion\\Analysis\\Jim Code")
setwd("C:\\Users\\Jim and Kristy\\Dropbox\\Experion\\Analysis\\Jim Code")
library(plyr)
library(splines)

# This loads bestmodel and bestmodel.summary
load( file="BestModel-Median-runs1-36.RData")
load( file="BestMeemirModel-Median-runs1-36.RData")
print(objects())


aggtab0 <- read.csv( file=paste('co2.features.BlindedTest3-median.csv', sep=''),
          header = TRUE)

colors <- unique(aggtab0$channel)


for( run in unique(aggtab0$run) ){
  print(paste("run",run))
  
  dat <- aggtab0[aggtab0$run==run,]
  df0 <- dat[dat$channel=="green",c("conc","resp","est.Conc.m","time","runtime")]
  df1 <- dat[dat$channel=="green",c("level.m","p2t.height.m","p2t.time.m","t2p.time.m")]
  df2 <- dat[dat$channel=="blue",c("level.m","p2t.height.m","p2t.time.m","t2p.time.m")]
  df3 <- dat[dat$channel=="red",c("level.m","p2t.height.m","p2t.time.m","t2p.time.m")]
  df4 <- dat[dat$channel=="clear",c("level.m","p2t.height.m","p2t.time.m","t2p.time.m")]
  colnames(df1) <- paste(colnames(df1),"green",sep=".")
  colnames(df2) <- paste(colnames(df2),"blue",sep=".")
  colnames(df3) <- paste(colnames(df3),"red",sep=".")
  colnames(df4) <- paste(colnames(df4),"clear",sep=".")
  aggtab <- data.frame(df0,df1,df2,df3,df4)
  


  aggtab <- within( aggtab, {
    for( color in colors ){
      assign(paste("p2t.height.over.sqtime.m",color,sep="."), get(paste("p2t.height.m",color,sep=".")) / get(paste("p2t.time.m",color,sep="."))^0.5 )
    }
    for( color in colors ){
      assign(paste("p2t.height.over.sqtime.over.level.m",color,sep="."), get(paste("p2t.height.m",color,sep=".")) / get(paste("p2t.time.m",color,sep="."))^0.5 / get(paste("level.m",color,sep=".")) )
    }
    for( color in 1:(length(colors)-1) ){
      assign(paste("ratio",colors[color],colors[color+1],sep="."), get(paste("level.m",colors[color],sep=".")) / get(paste("level.m",colors[color+1],sep=".")) )
    }
    for( color in 1:(length(colors)-1) ){
      assign(paste("log.ratio",colors[color],colors[color+1],sep="."), log(get(paste("level.m",colors[color],sep=".")) / get(paste("level.m",colors[color+1],sep="."))) )
    }
  })
  
  aggtab$prediction_respirion <- aggtab$est.Conc.m
  aggtab$prediction_meemir <- predict( bestmeemirmodel, aggtab)
  aggtab$prediction_combined <- predict( bestmodel, aggtab)
  
  # Format, name, and write feature table
  write.csv(aggtab,
            file=paste('co2.predictions.BlindedTest3_Run',run,'_median-calibrationRuns1-36.csv', sep=''),
            row.names=F)
  png(paste("predictionComaprison.BlindedTest3_Run",run,"_Median-calibrationRuns1-36.png",sep=""),height=400,width=600)
  plot(aggtab$runtime,aggtab$prediction_combined,ylim=c(0,60),type="l",bg="white",col="orange",lwd=2,xlab="time",ylab="Concentration",main=paste("Predictions for Blinded Test Run",run))
  lines(aggtab$runtime,aggtab$prediction_respirion,col="purple",lwd=2)
  lines(aggtab$runtime,aggtab$prediction_meemir,col="green",lwd=2)
  abline(a=0,b=0,col="grey")
  concs <- c(10,15,25,35,45,55,65)
  for(i in 1:6){abline(a=concs[i],b=0,col="lightgrey",lty=2,lwd=0.3)}
  legend(x="bottomleft",legend=c("Meemir","Respirion","Combined"),col=c("green","purple","orange"),lty=1,lwd=2)
  dev.off()
}