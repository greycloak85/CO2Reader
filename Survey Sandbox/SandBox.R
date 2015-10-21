# The intent of this script is to serve as a sandbox for approaches we might take 
# in the analysis of survey data to identify groups of individuals which share many 
# common features. 
#
# Last Updated on October 21, 2015


rm(list = ls(all = TRUE))  # Equivalent to "Clear All" in Matlab

setwd('~/Git/CO2Reader/Survey Sandbox') # Specify working path
dir()

# Define the variable types associated with the survey
questions=10;
answerVals=c(1:5);
respondents=200;

#generate answers to each question for each respondent
surveyResults=matrix(sample.int(max(answerVals), size = questions*respondents, replace = TRUE), nrow = questions, ncol = respondents)


#####################Below are questions to answer###################################


#*******Are the responses random?  (i.e. not spoofed, honestly generated, is there structure)*********



#*******What is the maximal group of questions such the N consistent responses are produced?**********
# order questions by variability

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
