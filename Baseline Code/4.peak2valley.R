require(reshape2)
require(ggplot2)
require(plyr)
require(zoo)

setwd("C:\\Users\\notebook\\Dropbox\\Experion\\Analysis\\Jim Code")

# Plotting Parameters
plotsize = 1200 # pixels
golden = 1.618 # ratio
res = 150
t.start <- 0
t.stop <- 3000

df <- read.csv(file='C:\\Users\\notebook\\Dropbox\\Experion\\Analysis\\Rich Code\\co2-moving-window-average-0-3000s-median.csv', header=T, sep = ',')

df <- df[!df$run %in% c(90,91),]

# SMOOTH DATA HERE. LOOK AT SMOOTH FUNCTION.  

# CALCULATE DERIVATIVES. 2-SIDED.

# CALCULATE TROUGH TO PEAK HEIGHT, MEAN DERIVATIVE

# Subset data.
dfsub0 <- df[complete.cases(df), ] # Drop incomplete rows with NAs

# Melt data to collapse channels into a single column
dfsub1 <- melt(dfsub0,
              id.vars=c('run','conc','resp','time','runtime'),
              measure.vars = c('green.avg','blue.avg','red.avg','clear.avg'),
              variable.name = 'channel', value.name='intensity')

dfsub2 <- melt(dfsub0,
              id.vars=c('run','conc','resp','time','runtime','est.Conc','est.Resp'),
              measure.vars = c('green.s','blue.s','red.s','clear.s'),
              variable.name = 'color', value.name='level')

dfsub <- cbind(dfsub1,dfsub2[,c('est.Conc','est.Resp','color','level')])

#---------------------------------------------------------------------------#
# Jim's peak-to-valley code from analysis.R
#---------------------------------------------------------------------------#
# Function for checking whether the central element in a vector is an extremum,
# and determining which kind.
# Returns +1 for a maximum, -1 for a minimum, and zero for any other point.
findlocalextremum <- function(v){
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
                          conc = character(0),
                          resp = character(0),
                          est.Conc = numeric(0),
                          est.Resp = numeric(0),
                          level.m = numeric(0),
                          level.sd = numeric(0),
                          p2t.height.m = numeric(0),
                          p2t.height.sd = numeric(0),
                          p2t.time.m = numeric(0),
                          p2t.time.sd = numeric(0),
                          t2p.time.m = numeric(0),
                          t2p.time.sd = numeric(0) )


for(i in unique(dfsub$run) ){
  print(i)
       for(j in c('green','blue','red','clear')){
         ch <- paste(j,"avg",sep=".")
         cl <- paste(j,"s",sep=".")
          print(paste(i,j))

          d <- subset(dfsub, run==i & channel==ch)
          
          d$extrema <- findextrema(d$intensity, width = width )

          # removes non-extrema observations
          d.e <- d[d$extrema!=0 & !is.na(d$extrema), ]

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
            features <- c(i, j, d$conc[1], d$resp[1],  
                          median(d$est.Conc[d$est.Conc>0 & d$est.Conc<100]), sd(d$est.Conc[d$est.Conc>0 & d$est.Conc<100]),
                          median(d$level),sd(d$level),
                          NA,NA,
                          NA,NA,
                          NA,NA)
            stop("nope")
            df.features <- rbind(df.features, t(features))           
          }else{

            #height differences
            peak.to.trough.heights <- d.p2t$intensity[seq(2,np2t,by=2)] - d.p2t$intensity[seq(2,np2t,by=2)-1]
            trough.to.peak.heights <- d.t2p$intensity[seq(2,nt2p,by=2)] - d.t2p$intensity[seq(2,nt2p,by=2)-1]

            #time differences
            peak.to.trough.timeunits <- d.p2t$time[seq(2,np2t,by=2)] - d.p2t$time[seq(2,np2t,by=2)-1]
            trough.to.peak.timeunits <- d.t2p$time[seq(2,nt2p,by=2)] - d.t2p$time[seq(2,nt2p,by=2)-1]

            # Append feature data
            features <- c(i, j, d$conc[1], d$resp[1], 
                        median(d$est.Conc[d$est.Conc>0 & d$est.Conc<100]), sd(d$est.Conc[d$est.Conc>0 & d$est.Conc<100]),
                        median(d$level),sd(d$level),
                      median(peak.to.trough.heights), sd(peak.to.trough.heights),
                      median(peak.to.trough.timeunits), sd(peak.to.trough.timeunits),
                      median(trough.to.peak.timeunits), sd(trough.to.peak.timeunits))
            df.features <- rbind(df.features, t(features))
          
        }
    }
}


# Format, name, and write feature table
names <- c('run', 'channel', 'conc', 'resp', 
           'est.Conc.m', 'est.Conc.sd',
           'level.m', 'level.sd',
           'p2t.height.m', 'p2t.height.sd',
           'p2t.time.m', 'p2t.time.sd', 
           't2p.time.m', 't2p.time.sd')
names(df.features) <- names
df.features$run <- as.numeric(as.character(df.features$run))
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

write.csv(df.features,
          file=paste('co2.features-',t.start,'-',t.stop,'-median.csv', sep=''),
          row.names=F)
stop()
#---------------------------------------------------------------------------#

png("ExtremaIntervals.png", height=plotsize/golden, width=plotsize, res = res)
par(mfrow=c(2,2))
plot(peak.to.trough.heights,  type="l")
plot(trough.to.peak.heights,  type="l")
plot(peak.to.trough.timeunits,type="l")
plot(trough.to.peak.timeunits,type="l")
dev.off()


# Aggregate data
aggregate(intensity~channel+run+conc+resp, data=dfsub, mean)
