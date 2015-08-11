require(reshape2)
require(ggplot2)
require(plyr)
require(zoo)
require(caTools)

# Plotting Parameters
plotsize = 1200 # pixels
golden = 1.618 # ratio
res = 150

# Check machine and set path appropriately
if(Sys.info()["nodename"] == 'Turing.local'){
  setwd('/Users/rhy/Dropbox/Work/Meemir/Respirion/Analysis/Rich Code')
} else if (Sys.info()["nodename"] == 'Richs-MacBook-Air.local'){
  setwd('/Users/Rich_Yaxley/Dropbox (Personal)/Work/Meemir/Respirion/Analysis/Rich Code')
}

df <- read.csv(file='co2.csv', header=T)

'''2015-03-26 Meemir meeting

We need to smooth the data to numerically differentiate. Maybe average a small number of neighboring points and walk up. Or, Jim suggested Smoothing with wavelets.

Also, need to smooth the unknown dataset

Try taking several 1-min chunks rather than 0-3000 epoch

Calculate the mean trough to peak values. Drop the peak to trough values.

Predicting the unknown dataset

Rolling window of 3 to 4 breaths
'''

# Smooth data test
x <- df$red[200000:200530]
plot(x, col='black', cex=1.4)
lines(runmean(x,5), col='red', lwd = 5)
lines(runquantile(x, 9, probs=0.5), col='blue', lwd=3)
# runmed = Running medians 
# lines(runmed(x,11), col='green', lwd = 3)
# lines(runmed(x,9), col='blue')
# lines(smooth(x, kind='3RS3R', twiceit=T), col='purple')
# lines(smooth.spline(x), col='green')



# Smooth data with running mean
df$green.s <- NA
df$blue.s <- NA
df$red.s <- NA
df$clear.s <- NA

for(run in unique(df$run)){
  df[which(df$run==run), ]$green.s <- runmean(df[which(df$run==run), ]$green, 5)
  df[which(df$run==run), ]$red.s <- runmean(df[which(df$run==run), ]$red, 5)
  df[which(df$run==run), ]$blue.s <- runmean(df[which(df$run==run), ]$blue, 5)
  df[which(df$run==run), ]$clear.s <- runmean(df[which(df$run==run), ]$clear, 5)
}


# # Test moving average
# ts <- zoo(r)
# tsmean <- rollapply(ts[ ,c('green','blue','red','clear')], width=timepoints,
#                     by=1, fill = NA, align = 'right', by.column = TRUE,
#                     FUN=function(y){ as.numeric(y)[timepoints] - mean(as.numeric(y))} )
# 
# # Test moving average on subset of data from one run
# timepoints <- 240  # 240 * 5 = 1200 timepoints (1200/60) = 20 s
# testrun <- 3
# r <- subset(df, run==testrun)
# r <- subset(r, runtime > 0 & runtime < 1000)
# ts <- zoo(r)
# tsmean <- rollapply(ts[ ,c('green','blue','red','clear')], width=timepoints,
#                     by=1, fill = NA, align = 'right', by.column = TRUE,
#                     FUN=function(y){ as.numeric(y)[timepoints] - mean(as.numeric(y))} )
# z <- as.data.frame(tsmean)
# r$green.avg <- z$green
# r$blue.avg <- z$blue
# r$red.avg <- z$red
# r$clear.avg <- z$clear
# 
# # Melt all color variables into one "channel" column
# r.m <- melt(r, id.vars=c('run','runtime','conc','resp'),
#             measure.vars = c('green','blue','red','clear','green.avg','blue.avg',
#                              'red.avg','clear.avg'),
#             variable.name = 'channel', value.name='intensity')
# 
# fname <- paste("Plots/Line-Intensity-x-Color-Moving-Window-Run", testrun, ".png", sep='')
# ggplot(data=r.m, aes(x=runtime, y=intensity, group=channel, color=channel )) +
#      geom_line(size=1) + facet_grid(channel~.,scales = "free_y")
# dev.copy(png, fname, height=plotsize/golden, width=plotsize, res=res)
# dev.off()
# 
# # Melt the green and green.average variablesonly
# r.m <- melt(r, id.vars=c('run','runtime','conc','resp'),
#             measure.vars = c('green','green.avg'),
#             variable.name = 'channel', value.name='intensity')
# 
# fname <- paste("Plots/Line-Intensity-x-Color-Moving-Window-Run", testrun, "-green-only.png", sep='')
# r.m <- subset(r.m, runtime > 20 & runtime < 40)
# ggplot(data=r.m, aes(x=runtime, y=intensity, group=channel, color=channel )) +
#      geom_line(size=1) + facet_grid(channel~.,scales = "free_y")
# dev.copy(png, fname, height=plotsize/golden, width=plotsize, res=res)
# dev.off()


# Generate a moving window average for every run
timepoints <- 1200  # 240 * 5 = 1200 timepoints (1200/60 samples/sec) = 20 s
df.m <- data.frame() #NULL
system.time(
     for(i in unique(df$run)){
          print(i)
          dfsub <- subset(df, run==i)
          dfsub <- subset(dfsub, runtime >= 0 & runtime < 3000) # subset of samples
          ts <- zoo(dfsub)
          
          #I put the na.rm=TRUE in the mean() below to cleanly handle NAs -- Jim
#           tsmean <- rollapply(ts[ ,c('green.s','blue.s','red.s','clear.s')], width=timepoints,
#                               by=1, align = 'right', by.column = TRUE, fill = NA,
#                               FUN=function(x){ as.numeric(x)[timepoints] - mean(as.numeric(x),na.rm=TRUE)})
          
          tsmean <- rollapply(ts[ ,c('green.s','blue.s','red.s','clear.s')], width=timepoints,
                              by=1, align = 'center', by.column = TRUE, fill = NA,
                              FUN=function(x){ as.numeric(x)[timepoints] - median(as.numeric(x),na.rm=TRUE)})

          z <- as.data.frame(tsmean)
          dfsub$green.avg <- z$green
          dfsub$blue.avg <- z$blue
          dfsub$red.avg <- z$red
          dfsub$clear.avg <- z$clear

          df.m <- rbind(df.m, dfsub)

          # Save each moving average to a var
          assign(paste('m.r', i, sep=''), dfsub)
     }
)
write.csv(df.m, file="co2-moving-window-average-0-3000s-median.csv", row.names=F)




