#setwd("C:\\Users\\notebook\\Dropbox\\Experion\\Analysis\\Jim Code")
setwd("C:\\Users\\Jim and Kristy\\Dropbox\\Experion\\Analysis\\Jim Code")
library(plyr)
library(boot)
library(cvTools)
library(splines)

aggtab0 <- read.csv("co2.features-0-3000-median.csv")
#aggtab0 <- aggtab0[aggtab0$run %in% 1:30,]

runs <- unique(aggtab0$run)
nruns <- length(runs)
colors <- gsub(".avg","",unique(aggtab0$channel))

aggtab <- ldply(.data = runs, .fun = function(q) {
  dat <- aggtab0[aggtab0$run==q,]
  df0 <- unique(dat[,c("conc","resp","est.Conc.m")])
  df1 <- dat[1,c("level.m","p2t.height.m","p2t.time.m","t2p.time.m")]
  df2 <- dat[2,c("level.m","p2t.height.m","p2t.time.m","t2p.time.m")]
  df3 <- dat[3,c("level.m","p2t.height.m","p2t.time.m","t2p.time.m")]
  df4 <- dat[4,c("level.m","p2t.height.m","p2t.time.m","t2p.time.m")]
  colnames(df1) <- paste(colnames(df1),"green",sep=".")
  colnames(df2) <- paste(colnames(df2),"blue",sep=".")
  colnames(df3) <- paste(colnames(df3),"red",sep=".")
  colnames(df4) <- paste(colnames(df4),"clear",sep=".")
  return(data.frame(df0,df1,df2,df3,df4))
})

aggtab <- within( aggtab, {
  for( color in colors ){
    assign(paste("p2p.time.m",color,sep="."), get(paste("p2t.time.m",color,sep=".")) + get(paste("t2p.time.m",color,sep=".")) )
    assign(paste("p2p.freq.m",color,sep="."), 1/get(paste("p2p.time.m",color,sep=".")) )
    assign(paste("p2t.freq.m",color,sep="."), 1/get(paste("p2t.time.m",color,sep=".")) )
    assign(paste("t2p.freq.m",color,sep="."), 1/get(paste("t2p.time.m",color,sep=".")) )
    assign(paste("p2p.time.m2",color,sep="."), get(paste("p2p.time.m",color,sep="."))^2 )
    assign(paste("p2t.time.m2",color,sep="."), get(paste("p2t.time.m",color,sep="."))^2 )
    assign(paste("t2p.time.m2",color,sep="."), get(paste("t2p.time.m",color,sep="."))^2 )
    assign(paste("p2p.height.over.time.m",color,sep="."), get(paste("p2t.height.m",color,sep=".")) / get(paste("p2p.time.m",color,sep=".")) )
    assign(paste("p2t.height.over.time.m",color,sep="."), get(paste("p2t.height.m",color,sep=".")) / get(paste("p2t.time.m",color,sep=".")) )
    assign(paste("t2p.height.over.time.m",color,sep="."), get(paste("p2t.height.m",color,sep=".")) / get(paste("t2p.time.m",color,sep=".")) )
    assign(paste("p2p.height2.over.time.m",color,sep="."), get(paste("p2t.height.m",color,sep="."))^2 / get(paste("p2p.time.m",color,sep=".")) )
    assign(paste("p2t.height2.over.time.m",color,sep="."), get(paste("p2t.height.m",color,sep="."))^2 / get(paste("p2t.time.m",color,sep=".")) )
    assign(paste("t2p.height2.over.time.m",color,sep="."), get(paste("p2t.height.m",color,sep="."))^2 / get(paste("t2p.time.m",color,sep=".")) )
    assign(paste("p2p.height.over.time2.m",color,sep="."), get(paste("p2t.height.m",color,sep=".")) / get(paste("p2p.time.m",color,sep="."))^2 )
    assign(paste("p2t.height.over.time2.m",color,sep="."), get(paste("p2t.height.m",color,sep=".")) / get(paste("p2t.time.m",color,sep="."))^2 )
    assign(paste("t2p.height.over.time2.m",color,sep="."), get(paste("p2t.height.m",color,sep=".")) / get(paste("t2p.time.m",color,sep="."))^2 )
    assign(paste("p2p.height.over.time.2.m",color,sep="."), (get(paste("p2t.height.m",color,sep=".")) / get(paste("p2p.time.m",color,sep=".")))^2 )
    assign(paste("p2t.height.over.time.2.m",color,sep="."), (get(paste("p2t.height.m",color,sep=".")) / get(paste("p2t.time.m",color,sep=".")))^2 )
    assign(paste("t2p.height.over.time.2.m",color,sep="."), (get(paste("p2t.height.m",color,sep=".")) / get(paste("t2p.time.m",color,sep=".")))^2 )
    assign(paste("p2p.height.over.time.3.m",color,sep="."), (get(paste("p2t.height.m",color,sep=".")) / get(paste("p2p.time.m",color,sep=".")))^3 )
    assign(paste("p2t.height.over.time.3.m",color,sep="."), (get(paste("p2t.height.m",color,sep=".")) / get(paste("p2t.time.m",color,sep=".")))^3 )
    assign(paste("t2p.height.over.time.3.m",color,sep="."), (get(paste("p2t.height.m",color,sep=".")) / get(paste("t2p.time.m",color,sep=".")))^3 )
    assign(paste("p2p.time.over.height.m",color,sep="."), get(paste("p2p.time.m",color,sep=".")) / get(paste("p2t.height.m",color,sep=".")) )
    assign(paste("p2t.time.over.height.m",color,sep="."), get(paste("p2t.time.m",color,sep=".")) / get(paste("p2t.height.m",color,sep=".")) )
    assign(paste("t2p.time.over.height.m",color,sep="."), get(paste("t2p.time.m",color,sep=".")) / get(paste("p2t.height.m",color,sep=".")) )
    assign(paste("p2p.time.times.height.m",color,sep="."), get(paste("p2p.time.m",color,sep=".")) * get(paste("p2t.height.m",color,sep=".")) )
    assign(paste("p2t.time.times.height.m",color,sep="."), get(paste("p2t.time.m",color,sep=".")) * get(paste("p2t.height.m",color,sep=".")) )
    assign(paste("t2p.time.times.height.m",color,sep="."), get(paste("t2p.time.m",color,sep=".")) * get(paste("p2t.height.m",color,sep=".")) )
    assign(paste("p2p.height.over.sqtime.m",color,sep="."), get(paste("p2t.height.m",color,sep=".")) / get(paste("p2p.time.m",color,sep="."))^0.5 )
    assign(paste("p2t.height.over.sqtime.m",color,sep="."), get(paste("p2t.height.m",color,sep=".")) / get(paste("p2t.time.m",color,sep="."))^0.5 )
    assign(paste("t2p.height.over.sqtime.m",color,sep="."), get(paste("p2t.height.m",color,sep=".")) / get(paste("t2p.time.m",color,sep="."))^0.5 )
    assign(paste("p2p.height.over.sqtime.3.m",color,sep="."), get(paste("p2t.height.m",color,sep="."))^3 / get(paste("p2p.time.m",color,sep="."))^1.5 )
    assign(paste("p2t.height.over.sqtime.3.m",color,sep="."), get(paste("p2t.height.m",color,sep="."))^3 / get(paste("p2t.time.m",color,sep="."))^1.5 )
    assign(paste("t2p.height.over.sqtime.3.m",color,sep="."), get(paste("p2t.height.m",color,sep="."))^3 / get(paste("t2p.time.m",color,sep="."))^1.5 )    
    assign(paste("p2p.height.over.sqtime.over.level.m",color,sep="."), get(paste("p2t.height.m",color,sep=".")) / get(paste("p2p.time.m",color,sep="."))^0.5 / get(paste("level.m",color,sep=".")) )
    assign(paste("p2t.height.over.sqtime.over.level.m",color,sep="."), get(paste("p2t.height.m",color,sep=".")) / get(paste("p2t.time.m",color,sep="."))^0.5 / get(paste("level.m",color,sep=".")) )
    assign(paste("t2p.height.over.sqtime.over.level.m",color,sep="."), get(paste("p2t.height.m",color,sep=".")) / get(paste("t2p.time.m",color,sep="."))^0.5 / get(paste("level.m",color,sep=".")) )
    assign(paste("p2p.height2.over.time.over.level.m",color,sep="."), get(paste("p2t.height.m",color,sep="."))^2 / get(paste("p2p.time.m",color,sep=".")) / get(paste("level.m",color,sep=".")) )
    assign(paste("p2t.height2.over.time.over.level.m",color,sep="."), get(paste("p2t.height.m",color,sep="."))^2 / get(paste("p2t.time.m",color,sep=".")) / get(paste("level.m",color,sep=".")) )
    assign(paste("t2p.height2.over.time.over.level.m",color,sep="."), get(paste("p2t.height.m",color,sep="."))^2 / get(paste("t2p.time.m",color,sep=".")) / get(paste("level.m",color,sep=".")) )
  }
  rm(color)
})


aggtab$log.level.m.green <- log(aggtab$level.m.green)
aggtab$log.level.m.blue <- log(aggtab$level.m.blue)
aggtab$log.level.m.red <- log(aggtab$level.m.red)
aggtab$log.level.m.clear <- log(aggtab$level.m.clear)
aggtab$ratio.green.blue <- aggtab$level.m.green/aggtab$level.m.blue
aggtab$ratio.blue.red <- aggtab$level.m.blue/aggtab$level.m.red
aggtab$ratio.red.clear <- aggtab$level.m.red/aggtab$level.m.clear
aggtab$log.ratio.green.blue <- log(aggtab$ratio.green.blue)
aggtab$log.ratio.blue.red <- log(aggtab$ratio.blue.red)
aggtab$log.ratio.red.clear <- log(aggtab$ratio.red.clear)

#holdout <- aggtab[21:nrow(aggtab),]
#aggtab <- aggtab[1:20,]

level <- paste("level.m",colors,sep=".",collapse="+")
log.level <- paste("log.level.m",colors,sep=".",collapse="+")
ns3.level <- paste("ns(level.m.",colors,",3)",sep="",collapse="+")
ns4.level <- paste("ns(level.m.",colors,",4)",sep="",collapse="+")
ratio <- "ratio.green.blue + ratio.blue.red + ratio.red.clear"
ns3.ratio <- "ns(ratio.green.blue,3) + ns(ratio.blue.red,3) + ns(ratio.red.clear,3)"
ns4.ratio <- "ns(ratio.green.blue,4) + ns(ratio.blue.red,4) + ns(ratio.red.clear,4)"
log.ratio <- "log.ratio.green.blue + log.ratio.blue.red + log.ratio.red.clear"
ns3.log.ratio <- "ns(log.ratio.green.blue,3) + ns(log.ratio.blue.red,3) + ns(log.ratio.red.clear,3)"
ns4.log.ratio <- "ns(log.ratio.green.blue,4) + ns(log.ratio.blue.red,4) + ns(log.ratio.red.clear,4)"
p2t.height <- paste("p2t.height.m",colors,sep=".",collapse="+")
p2p.time <- paste("p2p.time.m",colors,sep=".",collapse="+")
p2t.time <- paste("p2t.time.m",colors,sep=".",collapse="+")
t2p.time <- paste("t2p.time.m",colors,sep=".",collapse="+")
p2p.freq <- paste("p2p.freq.m",colors,sep=".",collapse="+")
p2t.freq <- paste("p2t.freq.m",colors,sep=".",collapse="+")
t2p.freq <- paste("t2p.freq.m",colors,sep=".",collapse="+")
p2p.time2 <- paste("p2p.time.m2",colors,sep=".",collapse="+")
p2t.time2 <- paste("p2t.time.m2",colors,sep=".",collapse="+")
t2p.time2 <- paste("t2p.time.m2",colors,sep=".",collapse="+")
p2p.height.over.time <- paste("p2p.height.over.time.m",colors,sep=".",collapse="+")
p2t.height.over.time <- paste("p2t.height.over.time.m",colors,sep=".",collapse="+")
t2p.height.over.time <- paste("t2p.height.over.time.m",colors,sep=".",collapse="+")
p2p.height2.over.time <- paste("p2p.height2.over.time.m",colors,sep=".",collapse="+")
p2t.height2.over.time <- paste("p2t.height2.over.time.m",colors,sep=".",collapse="+")
t2p.height2.over.time <- paste("t2p.height2.over.time.m",colors,sep=".",collapse="+")
p2p.height.over.time2 <- paste("p2p.height.over.time2.m",colors,sep=".",collapse="+")
p2t.height.over.time2 <- paste("p2t.height.over.time2.m",colors,sep=".",collapse="+")
t2p.height.over.time2 <- paste("t2p.height.over.time2.m",colors,sep=".",collapse="+")
p2p.height.over.time.2 <- paste("p2p.height.over.time.2.m",colors,sep=".",collapse="+")
p2t.height.over.time.2 <- paste("p2t.height.over.time.2.m",colors,sep=".",collapse="+")
t2p.height.over.time.2 <- paste("t2p.height.over.time.2.m",colors,sep=".",collapse="+")
p2p.height.over.time.3 <- paste("p2p.height.over.time.3.m",colors,sep=".",collapse="+")
p2t.height.over.time.3 <- paste("p2t.height.over.time.3.m",colors,sep=".",collapse="+")
t2p.height.over.time.3 <- paste("t2p.height.over.time.3.m",colors,sep=".",collapse="+")
p2p.time.over.height <- paste("p2p.time.over.height.m",colors,sep=".",collapse="+")
p2t.time.over.height <- paste("p2t.time.over.height.m",colors,sep=".",collapse="+")
t2p.time.over.height <- paste("t2p.time.over.height.m",colors,sep=".",collapse="+")
p2p.time.times.height <- paste("p2p.time.times.height.m",colors,sep=".",collapse="+")
p2t.time.times.height <- paste("p2t.time.times.height.m",colors,sep=".",collapse="+")
t2p.time.times.height <- paste("t2p.time.times.height.m",colors,sep=".",collapse="+")
p2p.height.over.sqtime <- paste("p2p.height.over.sqtime.m",colors,sep=".",collapse="+")
p2t.height.over.sqtime <- paste("p2t.height.over.sqtime.m",colors,sep=".",collapse="+")
t2p.height.over.sqtime <- paste("t2p.height.over.sqtime.m",colors,sep=".",collapse="+")
p2p.height.over.sqtime.3 <- paste("p2p.height.over.sqtime.3.m",colors,sep=".",collapse="+")
p2t.height.over.sqtime.3 <- paste("p2t.height.over.sqtime.3.m",colors,sep=".",collapse="+")
t2p.height.over.sqtime.3 <- paste("t2p.height.over.sqtime.3.m",colors,sep=".",collapse="+")
p2p.height.over.sqtime.over.level <- paste("p2p.height.over.sqtime.over.level.m",colors,sep=".",collapse="+")
p2t.height.over.sqtime.over.level <- paste("p2t.height.over.sqtime.over.level.m",colors,sep=".",collapse="+")
t2p.height.over.sqtime.over.level <- paste("t2p.height.over.sqtime.over.level.m",colors,sep=".",collapse="+")
p2p.height2.over.time.over.level <- paste("p2p.height2.over.time.over.level.m",colors,sep=".",collapse="+")
p2t.height2.over.time.over.level <- paste("p2t.height2.over.time.over.level.m",colors,sep=".",collapse="+")
t2p.height2.over.time.over.level <- paste("t2p.height2.over.time.over.level.m",colors,sep=".",collapse="+")




### This is my homebrew code that returns cross-validation predictions
### as well as prediction summary statistics and regresssion coefficients.
library(plyr)
crossvalidation <- function( data, folds, form, fittype = "lm", useweights = FALSE, family = "gaussian", link = NULL ){
  if(nrow(data)!=length(folds)) stop("folds and data dont match")
  nfolds <- unique(folds)
  if(useweights){wtstrng <-", weights = abs(p2t.height.m)^(-2)"}else{wtstrng<-NULL}
  if(!is.null(link)){lnkstrng <- paste(", family = ",family,"(link  = ",link,")")}else{lnkstrng<-NULL}  
  txt <- paste(fittype,"( form, data = data[folds!=q,]",wtstrng,lnkstrng,")",sep="")
  resp <- data[,"resp"]
  cv <- llply( .data = nfolds, .fun = function(q) {
    lmm <- eval(parse(text=txt))
    lmp <- predict( lmm, newdata = data[ folds == q, ])
    list(lmp,lmm$coef)
    })
  
  pre <- as.vector(unlist(lapply(cv,function(q) q[[1]])))
  obs <- data[,all.vars(form)[1]]
  res <- obs - pre 
  rmspe <- sqrt(mean(res^2))
  mpbia <- mean(res)
  mabpb <- mean(abs(res))
  negtv <- mean(pre<0)
  mat <-  matrix( c(rmspe,mpbia,mabpb,negtv) ,1,4)
  colnames(mat) <- c("RMSPE","MPBIA","MABPB","NEGTV")
  formtxt <- paste(as.character(form)[c(2,1,3)],collapse=" ")
  rownames(mat) <- gsub(" ","",gsub("form",formtxt,gsub(", data = data[folds!=q,]","",txt,fixed=TRUE)))
  return(list(Comparison = data.frame(Observations = obs, Predictions = pre, RespirationRate = resp), 
              Stats = mat ))
}

folds <- 1:nrow(aggtab)#rep(1:(nrow(aggtab)/4),each=4)


form1 <- formula(paste("conc ~ ",p2t.height," + ",p2t.freq," + ",t2p.freq) )

m1lm <- crossvalidation(data=aggtab,folds=folds,form=form1)


#m1lmrob <- crossvalidation(data=aggtab,folds=folds,form=form1,fittype="lmrob")

m1glm <- crossvalidation(data=aggtab,folds=folds,form=form1,fittype="glm",link="log",family="Gamma")

form2 <- formula(paste("conc ~ ",p2t.height," + ",t2p.freq) )

m2lm <- crossvalidation(data=aggtab,folds=folds,form=form2)

m2lmrob <- crossvalidation(data=aggtab,folds=folds,form=form2,fittype="lmrob")

m2glm <- crossvalidation(data=aggtab,folds=folds,form=form1,fittype="glm",link="log",family="Gamma")

form3 <- formula(paste("conc ~ ",p2t.height," + ",p2t.freq) )

m3lm <- crossvalidation(data=aggtab,folds=folds,form=form3)

m3lmrob <- crossvalidation(data=aggtab,folds=folds,form=form3,fittype="lmrob")

m3glm <- crossvalidation(data=aggtab,folds=folds,form=form3,fittype="glm",link="log",family="Gamma")


form4 <- formula(paste("conc ~ ",p2t.height) )
m4lm <- crossvalidation(data=aggtab,folds=folds,form=form4)

form5 <- formula(paste("conc ~ ",p2p.time) )
m5lm <- crossvalidation(data=aggtab,folds=folds,form=form5)

form6 <- formula(paste("conc ~ ",p2t.time) )
m6lm <- crossvalidation(data=aggtab,folds=folds,form=form6)

form7 <- formula(paste("conc ~ ",t2p.time) )
m7lm <- crossvalidation(data=aggtab,folds=folds,form=form7)

form8 <- formula(paste("conc ~ ",p2p.freq) )
m8lm <- crossvalidation(data=aggtab,folds=folds,form=form8)

form9 <- formula(paste("conc ~ ",p2t.freq) )
m9lm <- crossvalidation(data=aggtab,folds=folds,form=form9)

form10 <- formula(paste("conc ~ ",t2p.freq) )
m10lm <- crossvalidation(data=aggtab,folds=folds,form=form10)

form11 <- formula(paste("conc ~ ",p2t.height," + ",p2p.time) )
m11lm <- crossvalidation(data=aggtab,folds=folds,form=form11)

form12 <- formula(paste("conc ~ ",p2t.height," + ",p2t.time) )
m12lm <- crossvalidation(data=aggtab,folds=folds,form=form12)

form13 <- formula(paste("conc ~ ",p2t.height," + ",t2p.time) )
m13lm <- crossvalidation(data=aggtab,folds=folds,form=form13)

form14 <- formula(paste("conc ~ ",p2p.time.times.height) )
m14lm <- crossvalidation(data=aggtab,folds=folds,form=form14)

form15 <- formula(paste("conc ~ ",p2t.time.times.height) )
m15lm <- crossvalidation(data=aggtab,folds=folds,form=form15)

form16 <- formula(paste("conc ~ ",t2p.time.times.height) )
m16lm <- crossvalidation(data=aggtab,folds=folds,form=form16)

form17 <- formula(paste("conc ~ ",p2p.time.over.height) )
m17lm <- crossvalidation(data=aggtab,folds=folds,form=form17)

form18 <- formula(paste("conc ~ ",p2t.time.over.height) )
m18lm <- crossvalidation(data=aggtab,folds=folds,form=form18)

form19 <- formula(paste("conc ~ ",t2p.time.over.height) )
m19lm <- crossvalidation(data=aggtab,folds=folds,form=form19)

form20 <- formula(paste("conc ~ ",p2p.height.over.time) )
m20lm <- crossvalidation(data=aggtab,folds=folds,form=form20)

form21 <- formula(paste("conc ~ ",p2t.height.over.time) )
m21lm <- crossvalidation(data=aggtab,folds=folds,form=form21)

form22 <- formula(paste("conc ~ ",t2p.height.over.time) )
m22lm <- crossvalidation(data=aggtab,folds=folds,form=form22)

form23 <- formula(paste("conc ~ ",p2t.height," + ",p2p.time," + ",p2p.time2) )
m23lm <- crossvalidation(data=aggtab,folds=folds,form=form23)

form24 <- formula(paste("conc ~ ",p2t.height," + ",p2t.time," + ",p2t.time2) )
m24lm <- crossvalidation(data=aggtab,folds=folds,form=form24)

form25 <- formula(paste("conc ~ ",p2t.height," + ",t2p.time," + ",t2p.time2) )
m25lm <- crossvalidation(data=aggtab,folds=folds,form=form25)

form26 <- formula(paste("conc ~ ",p2t.height," + ",p2p.time," + ",p2p.time.times.height) )
m26lm <- crossvalidation(data=aggtab,folds=folds,form=form26)

form27 <- formula(paste("conc ~ ",p2t.height," + ",p2t.time," + ",p2t.time.times.height) )
m27lm <- crossvalidation(data=aggtab,folds=folds,form=form27)

form28 <- formula(paste("conc ~ ",p2t.height," + ",t2p.time," + ",t2p.time.times.height) )
m28lm <- crossvalidation(data=aggtab,folds=folds,form=form28)

form29 <- formula(paste("conc ~ ",p2t.height," + ",p2p.time," + ",p2p.time.over.height) )
m29lm <- crossvalidation(data=aggtab,folds=folds,form=form29)

form30 <- formula(paste("conc ~ ",p2t.height," + ",p2t.time," + ",p2t.time.over.height) )
m30lm <- crossvalidation(data=aggtab,folds=folds,form=form30)

form31 <- formula(paste("conc ~ ",p2t.height," + ",t2p.time," + ",t2p.time.over.height) )
m31lm <- crossvalidation(data=aggtab,folds=folds,form=form31)

form32 <- formula(paste("conc ~ ",p2t.height," + ",p2p.time," + ",p2p.height.over.time) )
m32lm <- crossvalidation(data=aggtab,folds=folds,form=form32)

form33 <- formula(paste("conc ~ ",p2t.height," + ",p2t.time," + ",p2t.height.over.time) )
m33lm <- crossvalidation(data=aggtab,folds=folds,form=form33)

form34 <- formula(paste("conc ~ ",p2t.height," + ",t2p.time," + ",t2p.height.over.time) )
m34lm <- crossvalidation(data=aggtab,folds=folds,form=form34)

form35 <- formula(paste("conc ~ ",p2p.height2.over.time) )
m35lm <- crossvalidation(data=aggtab,folds=folds,form=form35)

form36 <- formula(paste("conc ~ ",p2t.height2.over.time) )
m36lm <- crossvalidation(data=aggtab,folds=folds,form=form36)

form37 <- formula(paste("conc ~ ",t2p.height2.over.time) )
m37lm <- crossvalidation(data=aggtab,folds=folds,form=form37)

form38 <- formula(paste("conc ~ ",p2p.height.over.time2) )
m38lm <- crossvalidation(data=aggtab,folds=folds,form=form38)

form39 <- formula(paste("conc ~ ",p2t.height.over.time2) )
m39lm <- crossvalidation(data=aggtab,folds=folds,form=form39)

form40 <- formula(paste("conc ~ ",t2p.height.over.time2) )
m40lm <- crossvalidation(data=aggtab,folds=folds,form=form40)

form41 <- formula(paste("conc ~ ",p2p.height.over.time," + ", p2p.height.over.time.2) )
m41lm <- crossvalidation(data=aggtab,folds=folds,form=form41)

form42 <- formula(paste("conc ~ ",p2t.height.over.time," + ", p2t.height.over.time.2) )
m42lm <- crossvalidation(data=aggtab,folds=folds,form=form42)

form43 <- formula(paste("conc ~ ",t2p.height.over.time," + ", t2p.height.over.time.2) )
m43lm <- crossvalidation(data=aggtab,folds=folds,form=form43)

form44 <- formula(paste("conc ~ ",p2p.height.over.time," + ", p2p.height.over.time.2," + ", p2p.height.over.time.3) )
m44lm <- crossvalidation(data=aggtab,folds=folds,form=form44)

form45 <- formula(paste("conc ~ ",p2t.height.over.time," + ", p2t.height.over.time.2," + ", p2t.height.over.time.3) )
m45lm <- crossvalidation(data=aggtab,folds=folds,form=form45)

form46 <- formula(paste("conc ~ ",t2p.height.over.time," + ", t2p.height.over.time.2," + ", t2p.height.over.time.3) )
m46lm <- crossvalidation(data=aggtab,folds=folds,form=form46)

form47 <- formula(paste("conc ~ ",p2p.height.over.sqtime) )
m47lm <- crossvalidation(data=aggtab,folds=folds,form=form47)

form48 <- formula(paste("conc ~ ",p2t.height.over.sqtime) )
m48lm <- crossvalidation(data=aggtab,folds=folds,form=form48)

form49 <- formula(paste("conc ~ ",t2p.height.over.sqtime) )
m49lm <- crossvalidation(data=aggtab,folds=folds,form=form49)

form50 <- formula(paste("conc ~ ",p2p.height.over.sqtime," + ", p2p.height2.over.time) )
m50lm <- crossvalidation(data=aggtab,folds=folds,form=form50)

form51 <- formula(paste("conc ~ ",p2t.height.over.sqtime," + ", p2t.height2.over.time) )
m51lm <- crossvalidation(data=aggtab,folds=folds,form=form51)

form52 <- formula(paste("conc ~ ",t2p.height.over.sqtime," + ", t2p.height2.over.time) )
m52lm <- crossvalidation(data=aggtab,folds=folds,form=form52)

form53 <- formula(paste("conc ~ ",p2p.height.over.sqtime," + ", p2p.height2.over.time," + ", p2p.height.over.sqtime.3) )
m53lm <- crossvalidation(data=aggtab,folds=folds,form=form53)

form54 <- formula(paste("conc ~ ",p2t.height.over.sqtime," + ", p2t.height2.over.time," + ", p2t.height.over.sqtime.3) )
m54lm <- crossvalidation(data=aggtab,folds=folds,form=form54)

form55 <- formula(paste("conc ~ ",t2p.height.over.sqtime," + ", t2p.height2.over.time," + ", t2p.height.over.sqtime.3) )
m55lm <- crossvalidation(data=aggtab,folds=folds,form=form55)

form56 <- formula(paste("conc ~ ",level) )
m56lm <- crossvalidation(data=aggtab,folds=folds,form=form56)

form57 <- formula(paste("conc ~ ",ratio) )
m57lm <- crossvalidation(data=aggtab,folds=folds,form=form57)

form58 <- formula(paste("conc ~ ",log.level) )
m58lm <- crossvalidation(data=aggtab,folds=folds,form=form58)

form59 <- formula(paste("conc ~ ",log.ratio) )
m59lm <- crossvalidation(data=aggtab,folds=folds,form=form59)

form60 <- formula(paste("conc ~ est.Conc.m") )
m60lm <- crossvalidation(data=aggtab,folds=folds,form=form60)

form61 <- formula(paste("conc ~ ",level,"+",p2t.height.over.sqtime) )
m61lm <- crossvalidation(data=aggtab,folds=folds,form=form61)

form62 <- formula(paste("conc ~ ",ratio,"+",p2t.height.over.sqtime) )
m62lm <- crossvalidation(data=aggtab,folds=folds,form=form62)

form63 <- formula(paste("conc ~ ",log.level,"+",p2t.height.over.sqtime) )
m63lm <- crossvalidation(data=aggtab,folds=folds,form=form63)

form64 <- formula(paste("conc ~ ",log.ratio,"+",p2t.height.over.sqtime) )
m64lm <- crossvalidation(data=aggtab,folds=folds,form=form64)

form65 <- formula(paste("conc ~ est.Conc.m +",p2t.height.over.sqtime) )
m65lm <- crossvalidation(data=aggtab,folds=folds,form=form65)

form66 <- formula(paste("conc ~ est.Conc.m +",p2t.height.over.sqtime,"+",level) )
m66lm <- crossvalidation(data=aggtab,folds=folds,form=form66)

form67 <- formula(paste("conc ~ est.Conc.m +",p2t.height.over.sqtime,"+",ratio) )
m67lm <- crossvalidation(data=aggtab,folds=folds,form=form67)

form68 <- formula(paste("conc ~ ",ns3.level) )
m68lm <- crossvalidation(data=aggtab,folds=folds,form=form68)

form69 <- formula(paste("conc ~ ",ns3.ratio) )
m69lm <- crossvalidation(data=aggtab,folds=folds,form=form69)

form70 <- formula(paste("conc ~ ",ns3.level,"+",p2t.height.over.sqtime) )
m70lm <- crossvalidation(data=aggtab,folds=folds,form=form70)

form71 <- formula(paste("conc ~ ",ns3.ratio,"+",p2t.height.over.sqtime) )
m71lm <- crossvalidation(data=aggtab,folds=folds,form=form71)

form72 <- formula(paste("conc ~ ",log.ratio,"+",p2t.height.over.sqtime.over.level) )
m72lm <- crossvalidation(data=aggtab,folds=folds,form=form72)

form73 <- formula(paste("conc ~ est.Conc.m +",p2t.height.over.sqtime.over.level) )
m73lm <- crossvalidation(data=aggtab,folds=folds,form=form73)

form74 <- formula(paste("conc ~ est.Conc.m +",p2t.height.over.sqtime.over.level,"+",level) )
m74lm <- crossvalidation(data=aggtab,folds=folds,form=form74)

form75 <- formula(paste("conc ~ est.Conc.m +",p2t.height.over.sqtime.over.level,"+",ratio) )
m75lm <- crossvalidation(data=aggtab,folds=folds,form=form75)

form76 <- formula(paste("conc ~ ",p2t.height.over.sqtime.over.level) )
m76lm <- crossvalidation(data=aggtab,folds=folds,form=form76)

form77 <- formula(paste("conc ~ ",p2t.height.over.sqtime.over.level,"+",level) )
m77lm <- crossvalidation(data=aggtab,folds=folds,form=form77)

form78 <- formula(paste("conc ~ ",p2t.height.over.sqtime.over.level,"+",ratio) )
m78lm <- crossvalidation(data=aggtab,folds=folds,form=form78)

form79 <- formula(paste("conc ~ ns(",p2t.height.over.sqtime.over.level,",3)" ) )
m79lm <- crossvalidation(data=aggtab,folds=folds,form=form79)

form80 <- formula(paste("conc ~ ns(",p2t.height.over.sqtime.over.level,",3) +",level) )
m80lm <- crossvalidation(data=aggtab,folds=folds,form=form80)

form81 <- formula(paste("conc ~ ns(",p2t.height.over.sqtime.over.level,",3) +",ratio) )
m81lm <- crossvalidation(data=aggtab,folds=folds,form=form81)

form82 <- formula(paste("conc ~ ns(",p2t.height.over.sqtime.over.level,",4)" ) )
m82lm <- crossvalidation(data=aggtab,folds=folds,form=form82)

form83 <- formula(paste("conc ~ ns(",p2t.height.over.sqtime.over.level,",4) +",level) )
m83lm <- crossvalidation(data=aggtab,folds=folds,form=form83)

form84 <- formula(paste("conc ~ ns(",p2t.height.over.sqtime.over.level,",4) +",ratio) )
m84lm <- crossvalidation(data=aggtab,folds=folds,form=form84)

form85 <- formula(paste("conc ~ ",p2t.height2.over.time.over.level) )
m85lm <- crossvalidation(data=aggtab,folds=folds,form=form85)

form86 <- formula(paste("conc ~ ",p2t.height2.over.time.over.level,"+",level) )
m86lm <- crossvalidation(data=aggtab,folds=folds,form=form86)

form87 <- formula(paste("conc ~ ",p2t.height2.over.time.over.level,"+",ratio) )
m87lm <- crossvalidation(data=aggtab,folds=folds,form=form87)

form88 <- formula(paste("conc ~ ",p2t.height2.over.time.over.level,"+",p2t.height.over.sqtime.over.level) )
m88lm <- crossvalidation(data=aggtab,folds=folds,form=form88)

form89 <- formula(paste("conc ~ ",p2t.height2.over.time.over.level,"+",p2t.height.over.sqtime.over.level,"+",level) )
m89lm <- crossvalidation(data=aggtab,folds=folds,form=form89)

form90 <- formula(paste("conc ~ ",p2t.height2.over.time.over.level,"+",p2t.height.over.sqtime.over.level,"+",ratio) )
m90lm <- crossvalidation(data=aggtab,folds=folds,form=form90)

form91 <- formula(paste("conc ~ ",p2t.height.over.sqtime.over.level,"+ns(",ratio,",4)") )
m91lm <- crossvalidation(data=aggtab,folds=folds,form=form91)

form92 <- formula(paste("conc ~ ",p2t.height.over.sqtime.over.level,"+ns(",log.ratio,",4)") )
m92lm <- crossvalidation(data=aggtab,folds=folds,form=form92)

form93 <- formula(paste("conc ~ ",p2t.height.over.sqtime.over.level,"+ns(",ratio,",5)") )
m93lm <- crossvalidation(data=aggtab,folds=folds,form=form93)

form94 <- formula(paste("conc ~ ",p2t.height.over.sqtime.over.level,"+ns(",log.ratio,",5)") )
m94lm <- crossvalidation(data=aggtab,folds=folds,form=form94)

form95 <- formula(paste("conc ~ ",p2t.height.over.sqtime.over.level,"+",ratio,"+",level) )
m95lm <- crossvalidation(data=aggtab,folds=folds,form=form95)

form96 <- formula(paste("conc ~ ",p2t.height.over.sqtime.over.level,"+",log.ratio,"+",level) )
m96lm <- crossvalidation(data=aggtab,folds=folds,form=form96)

form97 <- formula(paste("conc ~ ",p2t.height.over.sqtime.over.level,"+",ns3.ratio) )
m97lm <- crossvalidation(data=aggtab,folds=folds,form=form97)

form98 <- formula(paste("conc ~ ",p2t.height.over.sqtime.over.level,"+",ns3.ratio,"+",level) )
m98lm <- crossvalidation(data=aggtab,folds=folds,form=form98)

form99 <- formula(paste("conc ~ ",p2t.height.over.sqtime.over.level,"+",ns3.log.ratio) )
m99lm <- crossvalidation(data=aggtab,folds=folds,form=form99)

form100 <- formula(paste("conc ~ ",p2t.height.over.sqtime.over.level,"+",ns3.log.ratio,"+",level) )
m100lm <- crossvalidation(data=aggtab,folds=folds,form=form100)

form101 <- formula(paste("conc ~ ",p2t.height.over.sqtime.over.level,"+",ns4.ratio) )
m101lm <- crossvalidation(data=aggtab,folds=folds,form=form101)

form102 <- formula(paste("conc ~ ",p2t.height.over.sqtime.over.level,"+",ns4.ratio,"+",level) )
m102lm <- crossvalidation(data=aggtab,folds=folds,form=form102)

form103 <- formula(paste("conc ~ ",p2t.height.over.sqtime.over.level,"+",ns4.log.ratio) )
m103lm <- crossvalidation(data=aggtab,folds=folds,form=form103)

form104 <- formula(paste("conc ~ ",p2t.height.over.sqtime.over.level,"+",ns4.log.ratio,"+",level) )
m104lm <- crossvalidation(data=aggtab,folds=folds,form=form104)

stattab <- rbind( m1lm$Stats,
                  #m1lmrob$Stats, 
                  m1glm$Stats, 
                  m2lm$Stats, 
                  m2lmrob$Stats,  
                  m2glm$Stats, 
                  m3lm$Stats, 
                  m3lmrob$Stats,  
                  m3glm$Stats, 
                  m4lm$Stats,
                  m5lm$Stats,
                  m6lm$Stats,
                  m7lm$Stats,
                  m8lm$Stats,
                  m9lm$Stats,
                  m10lm$Stats,
                  m11lm$Stats,
                  m12lm$Stats,
                  m13lm$Stats,
                  m14lm$Stats,
                  m15lm$Stats,
                  m16lm$Stats,
                  m17lm$Stats,
                  m18lm$Stats,
                  m19lm$Stats,
                  m20lm$Stats,
                  m21lm$Stats,
                  m22lm$Stats,
                  m23lm$Stats,
                  m24lm$Stats,
                  m25lm$Stats,
                  m26lm$Stats,
                  m27lm$Stats,
                  m28lm$Stats,
                  m29lm$Stats,
                  m30lm$Stats,
                  m31lm$Stats,
                  m32lm$Stats,
                  m33lm$Stats,
                  m34lm$Stats,
                  m35lm$Stats,
                  m36lm$Stats,
                  m37lm$Stats,
                  m38lm$Stats,
                  m39lm$Stats,
                  m40lm$Stats,
                  m41lm$Stats,
                  m42lm$Stats,
                  m43lm$Stats,
                  m44lm$Stats,
                  m45lm$Stats,
                  m46lm$Stats,
                  m47lm$Stats,
                  m48lm$Stats,
                  m49lm$Stats,
                  m50lm$Stats,
                  m51lm$Stats,
                  m52lm$Stats,
                  m53lm$Stats,
                  m54lm$Stats,
                  m55lm$Stats,
                  m56lm$Stats,
                  m57lm$Stats,
                  m58lm$Stats,
                  m59lm$Stats,
                  m60lm$Stats,
                  m61lm$Stats,
                  m62lm$Stats,
                  m63lm$Stats,
                  m64lm$Stats,
                  m65lm$Stats,
                  m66lm$Stats,
                  m67lm$Stats,
                  m68lm$Stats,
                  m69lm$Stats,
                  m70lm$Stats,
                  m71lm$Stats,
                  m72lm$Stats,
                  m73lm$Stats,
                  m74lm$Stats,
                  m75lm$Stats,
                  m76lm$Stats,
                  m77lm$Stats,
                  m78lm$Stats,
                  m79lm$Stats,
                  m80lm$Stats,
                  m81lm$Stats,
                  m82lm$Stats,
                  m83lm$Stats,
                  m84lm$Stats,
                  m85lm$Stats,
                  m86lm$Stats,
                  m87lm$Stats,
                  m88lm$Stats,
                  m89lm$Stats,
                  m90lm$Stats,
                  m91lm$Stats,
                  m92lm$Stats,
                  m93lm$Stats,
                  m94lm$Stats,
                  m95lm$Stats,
                  m96lm$Stats,
                  m97lm$Stats,
                  m98lm$Stats,
                  m99lm$Stats,
                  m100lm$Stats,
                  m101lm$Stats,
                  m102lm$Stats,
                  m103lm$Stats,
                  m104lm$Stats)


res <- aggtab$conc - aggtab$est.Conc.m 
rmspe <- sqrt(mean(res^2))
mpbia <- mean(res)
mabpb <- mean(abs(res))
negtv <- mean(aggtab$est.Conc.m<0)
mEst.Stats <-  matrix( c(rmspe,mpbia,mabpb,negtv) ,1,4)
colnames(mEst.Stats) <- c("RMSPE","MPBIA","MABPB","NEGTV")
rownames(mEst.Stats) <- "est.Conc.m"


stattab <- rbind( mEst.Stats, stattab ) 


crossvalidationLog <- function( data, folds, form ){
  if(nrow(data)!=length(folds)) stop("folds and data dont match")
  nfolds <- unique(folds)
  txt <- "lm( form, data = data[folds!=q,])"
  resp <- data[,"resp"]
  cv <- llply( .data = nfolds, .fun = function(q) {
    lmm <- eval(parse(text=txt))
    lmm$coef[is.na(lmm$coef)] <- 0
    lmp <- exp(predict( lmm, newdata = data[ folds == q, ]))
    list(lmp,lmm$coef)
  })
  
  pre <- as.vector(unlist(lapply(cv,function(q) q[[1]])))
  obs <- data[,all.vars(form)[1]]
  res <- obs - pre 
  rmspe <- sqrt(mean(res^2))
  mpbia <- mean(res)
  mabpb <- mean(abs(res))
  negtv <- mean(pre<0)
  mat <-  matrix( c(rmspe,mpbia,mabpb,negtv) ,1,4)
  colnames(mat) <- c("RMSPE","MPBIA","MABPB","NEGTV")
  formtxt <- paste(as.character(form)[c(2,1,3)],collapse=" ")
  rownames(mat) <- gsub(" ","",gsub("form",formtxt,gsub(", data = data[folds!=q,]","",txt,fixed=TRUE)))
  return(list(Comparison = data.frame(Observations = obs, Predictions = pre, RespirationRate = resp), 
              Stats = mat ))
}



form01lg <- formula(paste("log(conc) ~ ",level) )
m01lg <- crossvalidation(data=aggtab,folds=folds,form=form01lg)

form02lg <- formula(paste("log(conc) ~ ",ratio) )
m02lg <- crossvalidation(data=aggtab,folds=folds,form=form02lg)

form03lg <- formula(paste("log(conc) ~ est.Conc.m") )
m03lg <- crossvalidation(data=aggtab,folds=folds,form=form03lg)

stattab <- rbind( m01lg$Stats, 
                  m02lg$Stats,
                  m03lg$Stats,
                  stattab ) 

### Random Forest ###

library(randomForest)
crossvalidationRF <- function( data, folds, form, useweights = FALSE, family = "gaussian", link = NULL ){
  if(nrow(data)!=length(folds)) stop("folds and data dont match")
  nfolds <- unique(folds)
  if(useweights){wtstrng <-", weights = abs(p2t.height.m)^(-2)"}else{wtstrng<-NULL}
  if(!is.null(link)){lnkstrng <- paste(", family = ",family,"(link  = ",link,")")}else{lnkstrng<-NULL}  
  txt <- paste("randomForest( form, data = data[folds!=q,],importance=FALSE",wtstrng,lnkstrng,")",sep="")
  resp <- data[,"resp"]
  data <- data[,-which(colnames(data)=="resp")]
  cv <- llply( .data = nfolds, .fun = function(q) {
    rfm <- eval(parse(text=txt))
    lmp <- predict( rfm, newdata = data[ folds == q, ])
    list(lmp,rfm$importance)
  })
  
  pre <- as.vector(unlist(lapply(cv,function(q) q[[1]])))
  obs <- data[,all.vars(form)[1]]
  res <- obs - pre 
  rmspe <- sqrt(mean(res^2))
  mpbia <- mean(res)
  mabpb <- mean(abs(res))
  negtv <- mean(pre<0)
  mat <-  matrix( c(rmspe,mpbia,mabpb,negtv) ,1,4)
  colnames(mat) <- c("RMSPE","MPBIA","MABPB","NEGTV")
  formtxt <- paste(as.character(form)[c(2,1,3)],collapse=" ")
  rownames(mat) <- gsub(" ","",gsub("form",formtxt,gsub(", data = data[folds!=q,]","",txt,fixed=TRUE)))
  return(list(Comparison = data.frame(Observations = obs, Predictions = pre, RespirationRate = resp), 
              Stats = mat ))
}

m01rf <- crossvalidationRF(data=aggtab,folds=folds,form=formula(conc~.))

stattab <- rbind( m01rf$Stats, stattab ) 

### LASSO ###

library(glmnet)
crossvalidationLASSO <- function( data, folds ){
  if(nrow(data)!=length(folds)) stop("folds and data dont match")
  nfolds <- unique(folds) 
  resp <- data[,"resp"]
  data <- data[,-which(colnames(data)=="resp")]
  
  cv <- llply( .data = nfolds, .fun = function(q) {
    foldtmp <- as.vector(folds[folds != q])
    foldtmp[foldtmp>q] <- foldtmp[foldtmp>q]-1
    cv.lasso.out <- cv.glmnet( x = as.matrix(data[ folds != q, colnames(data)!="conc" ]), y = as.vector(data$conc[folds != q]), type.measure = "mse", foldid = foldtmp )
    coefs <- cv.lasso.out$glmnet.fit$beta[,which(cv.lasso.out$lambda == cv.lasso.out$lambda.min)]
    lmp <- predict(cv.lasso.out,newx=as.matrix(data[folds == q, colnames(data)!="conc"]), s="lambda.min")
    list(lmp,coefs)
  })
  
  pre <- as.vector(unlist(lapply(cv,function(q) q[[1]])))
  obs <- data[,"conc"] 
  res <- obs - pre 
  rmspe <- sqrt(mean(res^2))
  mpbia <- mean(res)
  mabpb <- mean(abs(res))
  negtv <- mean(pre<0)
  mat <-  matrix( c(rmspe,mpbia,mabpb,negtv) ,1,4)
  colnames(mat) <- c("RMSPE","MPBIA","MABPB","NEGTV")
  if(any(colnames(data)=="est.Conc.m")){
    formtxt <- "cv.glmnet( x = as.matrix(data[ , colnames(data)!=\"conc\" ]), y = as.vector(data$conc), type.measure = \"mse\", foldid = folds )"
  }else{
    formtxt <- "cv.glmnet( x = as.matrix(data[ , colnames(data)!=\"conc\" & colnames(data)!=\"est.Conc.m\" ]), y = as.vector(data$conc), type.measure = \"mse\", foldid = folds )"
  }
  rownames(mat) <- formtxt
  return(list(Comparison = data.frame(Observations = obs, Predictions = pre, RespirationRate = resp), 
              Stats = mat ))
}

m01la <- crossvalidationLASSO(data=aggtab,folds=folds)
m02la <- crossvalidationLASSO(data=aggtab[,colnames(aggtab)!="est.Conc.m"],folds=folds)

stattab <- rbind( m01la$Stats, 
                  m02la$Stats, 
                  stattab ) 


### SVM ###

library(e1071)
crossvalidationSVM <- function( data, folds, form, kern = "radial" ){
  if(nrow(data)!=length(folds)) stop("folds and data dont match")
  nfolds <- unique(folds)
  txt <- paste("svm( form, data = data[folds!=q,], kernel = \"",kern,"\")",sep="")
  resp <- data[,"resp"]
  data <- data[,-which(colnames(data)=="resp")]
  cv <- llply( .data = nfolds, .fun = function(q) {
    rfm <- eval(parse(text=txt))
    lmp <- predict( rfm, newdata = data[ folds == q, ])
    list(lmp)
  })
  
  pre <- as.vector(unlist(lapply(cv,function(q) q[[1]])))
  obs <- data[,all.vars(form)[1]]
  res <- obs - pre 
  rmspe <- sqrt(mean(res^2))
  mpbia <- mean(res)
  mabpb <- mean(abs(res))
  negtv <- mean(pre<0)
  mat <-  matrix( c(rmspe,mpbia,mabpb,negtv) ,1,4)
  colnames(mat) <- c("RMSPE","MPBIA","MABPB","NEGTV")
  formtxt <- paste(as.character(form)[c(2,1,3)],collapse=" ")
  rownames(mat) <- gsub(" ","",gsub("form",formtxt,gsub(", data = data[folds!=q,]","",txt,fixed=TRUE)))
  return(list(Comparison = data.frame(Observations = obs, Predictions = pre, RespirationRate = resp),  
              Stats = mat ))
}

m01sv <- crossvalidationSVM(data=aggtab,folds=folds,form=formula(conc~.),kern="linear")
m02sv <- crossvalidationSVM(data=aggtab,folds=folds,form=formula(conc~.),kern="polynomial")
m03sv <- crossvalidationSVM(data=aggtab,folds=folds,form=formula(conc~.),kern="radial")
m04sv <- crossvalidationSVM(data=aggtab,folds=folds,form=formula(conc~.),kern="sigmoid")

stattab <- rbind( m01sv$Stats, 
                  m02sv$Stats, 
                  m03sv$Stats, 
                  m04sv$Stats, 
                  stattab ) 



#Summarize
print(stattab)
stattab<-stattab[order(stattab[,"RMSPE"]),]
print(head(stattab))
print(head(stattab[order(abs(stattab[,"MPBIA"])),]))
print(head(stattab[order(stattab[,"MABPB"]),]))

write.csv( x = stattab, file = "PredictiveStatistics-0-3000-median-runs1-36.v3.csv" )

bestmodel <- with(data=aggtab,eval(parse(text=gsub("data","aggtab",rownames(stattab)[3]))))
bestmodel.summary <- summary(bestmodel)
save( bestmodel, bestmodel.summary,file="BestModel-Median-runs1-36.RData")

bestmeemirmodel <- with(data=aggtab,eval(parse(text=gsub("data","aggtab",rownames(stattab)[1]))))
bestmeemirmodel.summary <- summary(bestmeemirmodel)
save( bestmeemirmodel, bestmeemirmodel.summary,file="BestMeemirModel-Median-runs1-36.RData")

stop("stop")
