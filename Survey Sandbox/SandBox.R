#This intent of this script is to serve as a sandbox for approaches we might take in the analysis of survey data
# September 23, 2015
#Jim's comment
rm(list = ls(all = TRUE))  #equivalent to clear all in Matlab


#define the characteristices associated with the survey
questions=10;
answerVals=c(1:5);
respondents=200;



#generate answers to each question for each respondent
surveyResults=matrix(sample.int(max(answerVals), size = questions*respondents, replace = TRUE), nrow = questions, ncol = respondents)


#####################Below are questions to answer###################################


#*******Are the responses random?  (i.e. not spoofed, honestly generated, is there structure)*********



#*******What is the maximal group of questions such the N consistent responses are produced?**********

#Begin with a tree where the nodes contain all questions and each node denotes as specific combination of 
#answers to all questions.  The value at the node is the number of people who responded with that specific
#combination of answers.

#Next, remove a question (walk one level up the tree) such that the maximal node value is produced in the new layer.

#Repeat until a node value in the new layer is >=N.





#*******What are the 2 groups of questions such the N_1+N_2>=N consistent responses are produced?*****



#What are the M groups of questions such that sum(N_i, 1, M)>=N consistent responses are produced?
