# The intent of this script is to serve as a sandbox for approaches we might take 
# in the analysis of survey data to identify groups of people that differ in 
# important ways associated with products, preferences, and behaviors. By 
# understanding the differences among groups we can make better strategic choices
# about opportunities, product definition, and positioning, and can engage in 
# more effective promotion. 

# Last Updated on October 28, 2015

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
numResp = 500 # A row for each respondent

# Generate random answers to each question for each respondent
set.seed(18)
m <- matrix(rbinom(numQuest*numResp, maxVal, .5), ncol=numQuest)
df <- as.data.frame(m)

id <- sample(c('0':'9999999'), numResp, replace=F)
age <- sample(18:65, numResp, replace=TRUE)
sex <- sample(c('M','F'), numResp, replace=T)

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

seg.dist <- daisy(df)
seg.hc <- hclust(seg.dist, method='complete')
plot(seg.hc)
# Zoom in
plot(cut(as.dendrogram(seg.hc), h=0.35)$lower[[1]])


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
