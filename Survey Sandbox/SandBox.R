# The intent of this script is to serve as a sandbox for approaches we might take 
# in the analysis of survey data to identify groups of people that differ in 
# important ways associated with products, preferences, and behaviors. By 
# understanding the differences among groups we can make better strategic choices
# about opportunities, product definition, and positioning, and can engage in 
# more effective promotion. 

# Last Updated on November 4, 2015

rm(list = ls(all = TRUE))  # Equivalent to "Clear All" in Matlab

# Load packages
require(dplyr)

# Set working path based on machine
syntactic.device <- Sys.info()["nodename"]
if(syntactic.device == 'Richs-MacBook-Air.local'){
  setwd('/Users/Rich_Yaxley/Dropbox (Personal)/Work/Git/CO2Reader/Survey Sandbox')
} else if (syntactic.device == 'JIMS_MACHINE'){
  setwd('C:/Users/Jim and Kristy/Dropbox/???')
} else if (syntactic.device == 'WILLIAM-PC'){
  setwd('C:/Users/William/Dropbox (Personal)/CO2Reader/Survey Sandbox')
}

dir() # List contents to make sure you are in the right directory

# Define the variable types associated with the survey
maxVal = 5 # Largest value for Likert scale question
numQuest = 15 # A column for each question
numResp = 100 # A row for each respondent

# Generate random answers to each question for each respondent
set.seed(18)
m <- matrix(rbinom(numQuest*numResp, maxVal, .5), ncol=numQuest)
df <- as.data.frame(m)

id <- sample(c('0':'9999999'), numResp, replace=F)
age <- sample(18:65, numResp, replace=TRUE)
sex <- sample(c('M','F'), numResp, replace=T)

# LOAD DATA
# open survey data from Habitat for Humanity study
# clean data 
# 

# Combine columns, and question matrix
df <- cbind(id, age, sex, df)
head(df)

# Convert to a local data frame for prettier printing. Just experimenting with this.
# df <- tbl_df(df) 

# Calculate variability of each question
# variance <- apply(df, 2, sd) # Calculate SD for each column
# order(variance)
# Sort columns by variance in increasing order (Left=low variance, Right=high variance)
# For each row in a data frame
# Extract all rows that match 


# Unsupervised Clustering techniques

# Hierarchical Clustering: hclust()
# hclust uses distance measure
require(cluster)

seg.dist <- daisy(df) # distance function
as.matrix(seg.dist)[1:7,1:7]
seg.hc <- hclust(seg.dist, method='complete') 
plot(seg.hc)
# Zoom in
plot(cut(as.dendrogram(seg.hc), h=9)$lower[[1]])

# Check the similarity of a few pairs
df[c(45, 77), ] # similar
df[c(1, 31), ] # similar
df[c(45, 31), ] # dissimilar

# Check goodness-of-fit with the Cophenetic correlation coefficient (CPCC)
cophenetic(seg.hc)
cor(cophenetic(seg.hc), seg.dist)

# Determine how many groups we want for the segmentation
plot(seg.hc)
# Cut at 4 groups
rect.hclust(seg.hc, k=4, border='red')
seg.hc.segment <- cutree(seg.hc, k=4) # Vector with group membership labels
table(seg.hc.segment) # Table with size of segments

# plot(jitter(as.numeric(df$V1)) ~ 
#      jitter(as.numeric(df$V2)),
#      col=seg.hc.segment, yaxt='n', xact='n', ylab='', xlab='')






# Mean-based Clustering: kmeans()
# kmeans uses numeric data
dfNums=df[4:ncol(df)]
dfNumKmeans=kmeans(dfNums,4)
daisyDFNums=daisy(dfNums)
sil   <- silhouette(dfNumKmeans$cl, daisyDFNums^2)
plot(sil)
plotcluster(dfNums,dfNumKmeans$cl)


# Model-based Clustering: MClust()
# MClust uses numeric data


# Comparing Models with Bayesian information criterion: BIC()


# Latent class analysis: poLCA()
# poLCA uses only categorical variables










#---------------------------------------------------------------------------#
# Open Questions
#---------------------------------------------------------------------------#

# Q: Are the responses random?  (i.e. not spoofed, honestly generated, is there structure)
# I think it would be best to assume that the dataset is genuine and any problematic
# participants have been excluded in preprocessing steps. If we want, down the road
# we could implement a few  basic checks to ensure that data at least appears to be 
# reasonable.


# Q: What is the maximal group of questions such the N consistent responses are produced?
# Unsure

#simpler!
#make a temporary matrix matTemp

#for remCol=0:n # take out none, take out 1, take out 1&2, take out 1, 2, &3, etc....  Question order must be arranged in pre-processing
  #if remCol==0
    #continue
  #else
  #remove column number remCol corresponding to question number remCol
  #end

  #for thisRow=1:m
    #find the number of rows = to row thisRow, save in a vector of length m called commonAns
    #remove rows that are = to this row, or skip elements of row that match this one.
  #end
  #check if any val in commonAns>N (desired threshold)
  #if so, break
#end



#*******What are the 2 groups of questions such the N_1+N_2>=N consistent responses are produced?*****



#What are the M groups of questions such that sum(N_i, 1, M)>=N consistent responses are produced?




Answers <- surveyResults
rownames(Answers) <- paste("Resp_",1:numRespondents,sep="")
colnames(Answers) <- paste("Q_",1:numQuestions,sep="") 

#############################################################
### Simultaneous hclust on both respondents and questions ###
#############################################################
library(gplots)
heatmap.2(Answers)

###############################################
###       Clustering respondents            ###
### Use t(Answers) to cluster on questions. ###
### Pick distance metric, agglomeration     ###
### method, and index (measure of fit).     ###
###############################################
library(NbClust) #basically every clustering method that exists is in this package
# euclidean (L_2) distance
results.1 <- NbClust(Answers, distance = "euclidean", min.nc = 2, max.nc = 15, method = "ward.D2", index = "alllong" )

# manhattan (L_1) distance
results.2 <- NbClust(Answers, distance = "manhattan", min.nc = 2, max.nc = 15, method = "ward.D2", index = "alllong" )

# canberra distance
results.3 <- NbClust(Answers, distance = "canberra", min.nc = 2, max.nc = 15, method = "ward.D2", index = "alllong" )

# Maximum (Chebyshev, L_inf) distance
results.4 <- NbClust(Answers, distance = "maximum", min.nc = 2, max.nc = 15, method = "ward.D2", index = "alllong" )
