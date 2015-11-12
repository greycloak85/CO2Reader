require(reshape2)
require(ggplot2)

# Data and analysis directory
setwd('/Users/Rich_Yaxley/Dropbox (Personal)/Work/Meemir/Lottery')

# Survey data from Critical Mix panel
df <- read.csv("survey.data.csv", header = T, na.strings='NA', skip=0, stringsAsFactors=F)

questions <- df[1, ]

# drop first row w/ names
df <- df[-1, ] 

# Remove unnecessary columns
drops <- c('V2', 'V3', 'V4', 'V5', 'V6', 'V7', 'V8', 'V9', 'External', 'Q1.1', 
           'LocationLatitude', 'LocationLongitude', 'LocationAccuracy', 'X',
           'Q11_1_TEXT',	'Q11_2_TEXT',	'Q11_3_TEXT',	'Q12_1_TEXT',	'Q12_2_TEXT',
           'Q12_3_TEXT',	'Q12_4_TEXT',	'Q12_5_TEXT',	'Q12_6_TEXT',	'Q12_7_TEXT',
           'Q12_8_TEXT',	'Q12_9_TEXT',	'Q12_10_TEXT',	'Q13',	'Q14_1_TEXT',	
           'Q14_2_TEXT',	'Q14_3_TEXT',	'Q14_4_TEXT',	'Q14_5_TEXT',	'Q14_6_TEXT',
           'Q14_7_TEXT',	'Q14_8_TEXT',	'Q14_9_TEXT', 'Q14_10_TEXT', 	'Q15', 	'Q16',
           'Q22_TEXT',	'Q23', 'Q30', 'Q31.1_17_TEXT', 'Q31.2_17_TEXT', 'Q31.3_17_TEXT',
           'Q34', 'Q47', 'Q49', 'Q50', 'Q63_9_TEXT')
df <- df[ , !(names(df) %in% drops)]
questions <- questions[ , !(names(questions) %in% drops)]

# Rename columns
colnames(df)[1] <- 'ID'

# Drop incompletes
df <- subset(df, V10 == 1) # OR data <- subset(data, Complete == 'Good')

head(df)

write.csv(df, file='survey.data.clean.csv', row.names=F)



#---------------------------------------------------------------------------# 
# keeps <- c('ID', 'Q7', 'Q35_1', 'Q35_2', 'Q35_3', 'Q35_4', 'Q35_5', 'Q35_6', 'Q35_7', 'Q35_8', 'Q35_9',
#            'Q35_10', 'Q35_11', 'Q35_12', 'Q35_13', 'Q35_14', 'Q35_15', 'Q35_16', 'Q35_17', 'Q35_18', 'Q35_19',
#            'Q35_20', 'Q35_21', 'Q35_22', 'Q35_23', 'Q35_24', 'Q35_25', 'Q35_26', 'Q35_27', 'Q35_28')
# df <- df[ , (names(df) %in% keeps)]




# Melt data into long format
# df <- melt(df, id.vars=c('ID','Q7'), variable.name = 'Perceptions', value.name= 'Rating')




# # General Attitudes questions from Q7.2_1--Q7.4_12
# keeps <- c( 'Q7.2_1', 'Q7.2_2', 'Q7.2_3', 'Q7.2_4', 'Q7.2_5', 'Q7.2_6', 'Q7.2_7', 'Q7.2_8', 'Q7.2_9', 'Q7.2_10', 'Q7.2_11', 'Q7.2_12', 'Q7.2_13', 'Q7.2_14', 'Q7.3_1', 'Q7.3_2', 'Q7.3_3',
#             'Q7.3_4', 'Q7.3_5', 'Q7.3_6', 'Q7.3_7', 'Q7.3_8', 'Q7.3_9', 'Q7.3_10', 'Q7.3_11', 'Q7.3_12', 'Q7.3_13', 'Q7.3_14', 'Q7.4_1', 'Q7.4_2', 'Q7.4_3', 'Q7.4_4', 'Q7.4_5', 'Q7.4_6', 'Q7.4_7',
#             'Q7.4_8', 'Q7.4_9', 'Q7.4_10', 'Q7.4_11', 'Q7.4_12')
# ga <- df[ , (names(df) %in% keeps)]
# ga <- sapply(ga, as.numeric) # Do I want a factor or integer here???
# ga <- data.frame(ga)
# 
# # PCA
# ga.pca <- prcomp(ga)
# ga.pca$sdev
# 
# screeplot(ga.pca, type='lines')
# 
# # Kaiser's criterion - keep principal components for which variance > 1
# (ga.pca$sdev)^2






# # Recode age  
# require(car)
# df2$AgeGroup  <- recode(df2$Age, "18:24='18-24'; 
#                                   25:34='25-34';
#                                   35:44='35-44';
#                                   45:54='45-54';
#                                   55:64='55-64';
#                                   65:99='65+' ")
# 
# # Segment data
# # LIFESTYLE CODES  
# # 1	Confident, Opinionated Organizers
# # 2	Money Matters
# # 3	Experiential Driven Non-conformists
# # 4	Religious
# # 5	Global Outlook
# # 
# # CHARITY CODES	
# # 1	Charity/Giving Important
# # 2	Charity/Giving Less Important
# 
# segments <- read.csv("Data/HfH Segment Respondent Level Data.csv")
# colnames(segments)[1] <- 'ID'
# 
# # Merge datasets
# df2 <- merge(df2, segments, by = 'ID')
# 
# df2$Organizers <- NA
# df2$Money.Focused <- NA
# df2$Nonconformists <- NA
# df2$Religious <- NA
# df2$Global.Outlook <- NA
# df2$Organizers[which(df2$Lifestyle_Seg==1)] <- 1
# df2$Money.Focused[which(df2$Lifestyle_Seg==2)] <- 1
# df2$Nonconformists[which(df2$Lifestyle_Seg==3)] <- 1
# df2$Religious[which(df2$Lifestyle_Seg==4)] <- 1
# df2$Global.Outlook[which(df2$Lifestyle_Seg==5)] <- 1
# 
# df2$Giving.Important <- NA
# df2$Giving.Less.Important <- NA
# df2$Giving.Important[which(df2$Charity_Seg==1)] <- 1
# df2$Giving.Less.Important[which(df2$Charity_Seg==2)] <- 1
# 
# write.csv(df2, file='Data/Data-Habitat-Survey.csv', row.names=F)
# 
# #---------------------------------------------------------------------------#
# 
# lifestyle.age <- table(df2$AgeGroup, df2$Lifestyle_Seg)
# margin.table(lifestyle.age,1)
# margin.table(lifestyle.age,2)
# prop.table(lifestyle.age,2)
# 
# mytable <- xtabs(~AgeGroup+Lifestyle_Seg, data=df2)
# ftable(mytable)
# summary(mytable)
# 
# 
# library(gmodels)
# CrossTable(df2$AgeGroup, df2$Lifestyle_Seg)
# 
# CrossTable(df2$Gender, df2$Lifestyle_Seg)
# 
# CrossTable(df2$State, df2$Lifestyle_Seg)
