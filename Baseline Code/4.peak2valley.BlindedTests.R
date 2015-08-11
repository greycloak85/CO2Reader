require(reshape2)
require(ggplot2)
require(plyr)
require(zoo)

timepoints <- 1801 #30 seconds 

#setwd("C:\\Users\\notebook\\Dropbox\\Experion\\Analysis\\Jim Code")
setwd("C:\\Users\\Jim and Kristy\\Dropbox\\Experion\\Analysis\\Jim Code")

# Plotting Parameters
plotsize = 1200 # pixels
golden = 1.618 # ratio
res = 150

#df <- read.csv(file='C:\\Users\\notebook\\Dropbox\\Experion\\Analysis\\Rich Code\\co2-moving-window-average-0-3000s-median.csv', header=T, sep = ',')
df <- read.csv(file='C:\\Users\\Jim and Kristy\\Dropbox\\Experion\\Analysis\\Rich Code\\co2-moving-window-average-0-3000s-median.csv', header=T, sep = ',')

df <- df[df$run %in% c(92),]

# SMOOTH DATA HERE. LOOK AT SMOOTH FUNCTION.  

# CALCULATE DERIVATIVES. 2-SIDED.

# CALCULATE TROUGH TO PEAK HEIGHT, MEAN DERIVATIVE

# Subset data.
dfsub0 <- df#[complete.cases(df), ] # Drop incomplete rows with NAs

dfsub0$error <- as.integer(is.na(dfsub0$green) | is.na(dfsub0$blue) | is.na(dfsub0$red) | is.na(dfsub0$clear))

# Melt data to collapse channels into a single column
dfsub1 <- melt(dfsub0,
               id.vars=c('run','conc','resp','time','runtime'),
               measure.vars = c('green.avg','blue.avg','red.avg','clear.avg'),
               variable.name = 'channel', value.name='intensity')

dfsub2 <- melt(dfsub0,
               id.vars=c('run','conc','resp','time','runtime','est.Conc','est.Resp','error'),
               measure.vars = c('green.s','blue.s','red.s','clear.s'),
               variable.name = 'color', value.name='level')

dfsub <- cbind(dfsub1,dfsub2[,c('est.Conc','est.Resp','color','level','error')])

#---------------------------------------------------------------------------#
# Jim's peak-to-valley code from analysis.R
#---------------------------------------------------------------------------#
# Function for checking whether the central element in a vector is an extremum,
# and determining which kind.
# Returns +1 for a maximum, -1 for a minimum, and zero for any other point.
findlocalextremum <- function(v){
     if(any(is.na(v))){return(NA)}
     nv <- length(v)
     if (nv%%2==0 ){ mid <- (nv+2)/2 } else{  mid <- (nv+1)/2 }
     ret <- 0
     if(!is.unsorted( v[1:mid]) && !is.unsorted(-v[mid:nv])){ ret <-  1 }
     if(!is.unsorted(-v[1:mid]) && !is.unsorted( v[mid:nv])){ ret <- -1 }
     return(ret)
}

#Function to find all extrema in a long-ish vector
findextrema <- function(dat, width=5){
     ld <- length(dat)
     buf <- (width-1)/2
     xx <- sapply((1+buf):(ld-buf), function(q) findlocalextremum( dat[(q-buf):(q+buf)] ) )
     return(c(rep(NA,buf), xx, rep(NA,buf)))
}

# Size of window around each point with which to determine whether the
# central point is an extremum
width <- 7
# Create new column for extreme values
dfsub$extrema <- NA

# Create new dataframe to fill
df.features <- data.frame(run = character(0),
                          channel = character(0),
                          time = numeric(0),
                          runtime = numeric(0),
                          conc = character(0),
                          resp = character(0),
                          est.Conc.m = numeric(0),
                          est.Conc.sd = numeric(0),
                          level.m = numeric(0),
                          level.sd = numeric(0),
                          p2t.height.m = numeric(0),
                          p2t.height.sd = numeric(0),
                          p2t.time.m = numeric(0),
                          p2t.time.sd = numeric(0),
                          t2p.time.m = numeric(0),
                          t2p.time.sd = numeric(0) )


getstats <- function(i,j,dd,width){
  dd <- as.data.frame(dd)
  dd$conc <- as.numeric(as.character(dd$conc))
  dd$resp <- as.numeric(as.character(dd$resp))
  dd$time <- as.numeric(as.character(dd$time))
  dd$runtime <- as.numeric(as.character(dd$runtime))
  dd$intensity <- as.numeric(as.character(dd$intensity))
  dd$est.Conc <- as.numeric(as.character(dd$est.Conc))
  dd$est.Resp <- as.numeric(as.character(dd$est.Resp))
  dd$level <- as.numeric(as.character(dd$level))
  dd$extrema <- as.numeric(as.character(dd$extrema))
  nd <- nrow(dd)
  if(max(dd$time)%%60==0){print(paste(i,j,max(dd$runtime)))}

  if(any(is.na(dd$intensity))){
    feature <- data.frame(cbind(i, j, max(dd$time), max(dd$runtime), dd$conc[1], dd$resp[1], 
                                median(dd$est.Conc[dd$est.Conc>0 & dd$est.Conc<100]), sd(dd$est.Conc[dd$est.Conc>0 & dd$est.Conc<100]),
                                median(dd$level),sd(dd$level),
                  NA,NA,
                  NA,NA,
                  NA,NA))
    names <- c('run', 'channel', 'time', 'runtime', 'conc', 'resp', 
               'est.Conc.m', 'est.Conc.sd',
               'level.m', 'level.sd',
               'p2t.height.m', 'p2t.height.sd',
               'p2t.time.m', 'p2t.time.sd', 
               't2p.time.m', 't2p.time.sd')
    colnames(feature) <- names
    return(feature)
  }
  #print(max(dd$time))
  #dd$extrema <- findextrema(dd$intensity, width = width )
  #print(sum(!is.na(dd$extrema)))
  #print(d$extrema)
  
  # removes non-extrema observations
  d.e <- dd[dd$extrema!=0 & !is.na(dd$extrema), ]
  
  peaks   <- d.e$extrema ==  1
  troughs <- d.e$extrema == -1
  
  # Peak to Trough
  PT <- which( peaks & c(troughs[-1],FALSE) )
  PT <- sort( c(PT,PT+1) )
  d.p2t <- d.e[PT, ]
  
  # Trough to Peak
  TP <- which( troughs & c(peaks[-1],FALSE) )
  TP <- sort( c(TP,TP+1) )
  d.t2p <- d.e[TP, ]
  
  np2t <- nrow(d.p2t)
  nt2p <- nrow(d.t2p)
  if(np2t==0 || nt2p==0){
    feature <- data.frame(cbind(i, j, max(dd$time), max(dd$runtime), dd$conc[1], dd$resp[1], 
                                median(dd$est.Conc[dd$est.Conc>0 & dd$est.Conc<100]), sd(dd$est.Conc[dd$est.Conc>0 & dd$est.Conc<100]),
                                median(dd$level),sd(dd$level),
                  NA,NA,
                  NA,NA,
                  NA,NA))
    names <- c('run', 'channel', 'time', 'runtime', 'conc', 'resp', 
               'est.Conc.m', 'est.Conc.sd',
               'level.m', 'level.sd',
               'p2t.height.m', 'p2t.height.sd',
               'p2t.time.m', 'p2t.time.sd', 
               't2p.time.m', 't2p.time.sd')
    colnames(feature) <- names  
    return(feature)
  }
  #print(TP)
  #print(PT)
  #print(np2t)
  #print(nt2p)
  
  #height differences
  peak.to.trough.heights <- d.p2t$intensity[seq(2,np2t,by=2)] - d.p2t$intensity[seq(2,np2t,by=2)-1]
  trough.to.peak.heights <- d.t2p$intensity[seq(2,nt2p,by=2)] - d.t2p$intensity[seq(2,nt2p,by=2)-1]
  
  #time differences
  peak.to.trough.timeunits <- d.p2t$time[seq(2,np2t,by=2)] - d.p2t$time[seq(2,np2t,by=2)-1]
  trough.to.peak.timeunits <- d.t2p$time[seq(2,nt2p,by=2)] - d.t2p$time[seq(2,nt2p,by=2)-1]
  
  # Append feature data
  feature <- data.frame(cbind(i, j, max(dd$time), max(dd$runtime), dd$conc[1], dd$resp[1], 
                median(dd$est.Conc[dd$est.Conc>0 & dd$est.Conc<100]), sd(dd$est.Conc[dd$est.Conc>0 & dd$est.Conc<100]),
                median(dd$level),sd(dd$level),
                median(peak.to.trough.heights), sd(peak.to.trough.heights),
                median(peak.to.trough.timeunits), sd(peak.to.trough.timeunits),
                median(trough.to.peak.timeunits), sd(trough.to.peak.timeunits)))
  names <- c('run', 'channel', 'time', 'runtime', 'conc', 'resp', 
             'est.Conc.m', 'est.Conc.sd',
             'level.m', 'level.sd',
             'p2t.height.m', 'p2t.height.sd',
             'p2t.time.m', 'p2t.time.sd', 
             't2p.time.m', 't2p.time.sd')
  names(feature) <- names
  return(feature)
}

for(i in unique(dfsub$run) ){
  print(i)
  for(j in c('green','blue','red','clear')){
    ch <- paste(j,"avg",sep=".")
    cl <- paste(j,"s",sep=".")

          d <- subset(dfsub, run==i & channel==ch)
          
          d$extrema <- findextrema(d$intensity, width = width )

          ts <- zoo(d)
          
          features <- rollapply(ts, width=timepoints,
                                by=1, align = 'center', by.column = FALSE, #fill = NA,
                                FUN=function(q) getstats(i,j,q,width))

          df.features <- rbind(df.features, as.data.frame(features))
     }
}


df.features$run <- as.numeric(as.character(df.features$run))
df.features$time <- as.numeric(as.character(df.features$time))
df.features$runtime <- as.numeric(as.character(df.features$runtime))
df.features$conc <- as.numeric(as.character(df.features$conc))
df.features$resp <- as.numeric(as.character(df.features$resp))
df.features$est.Conc.m <- as.numeric(as.character(df.features$est.Conc.m))
df.features$est.Conc.sd <- as.numeric(as.character(df.features$est.Conc.sd))
df.features$level.m <- as.numeric(as.character(df.features$level.m))
df.features$level.sd <- as.numeric(as.character(df.features$level.sd))
df.features$p2t.height.m <- as.numeric(as.character(df.features$p2t.height.m))
df.features$p2t.height.sd <- as.numeric(as.character(df.features$p2t.height.sd))
df.features$p2t.time.m <- as.numeric(as.character(df.features$p2t.time.m))
df.features$p2t.time.sd <- as.numeric(as.character(df.features$p2t.time.sd))
df.features$t2p.time.m <- as.numeric(as.character(df.features$t2p.time.m))
df.features$t2p.time.sd <- as.numeric(as.character(df.features$t2p.time.sd))

# Format, name, and write feature table
write.csv(df.features,
          file=paste('co2.features.BlindedTest3-median.csv', sep=''),
          row.names=F)

#---------------------------------------------------------------------------#
