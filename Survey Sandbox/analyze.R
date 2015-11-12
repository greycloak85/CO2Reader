require(reshape2)
require(ggplot2)

setwd('/Users/Rich_Yaxley/Dropbox (PARAGRAPH)/2015 Projects/Mullen/NCEL/MUL1503_LOT NCEL Brand Tracking/Internal Share/Data Processing/Scripts')
/Users/Rich_Yaxley/Dropbox (Personal)/Work/Meemir/Lottery/survey.data.csv
# Survey data from Critical Mix panel
data <- read.csv("../Final Data Files/MUL1503_NCEL.csv", header = T, na.strings='NA', skip=0, stringsAsFactors=F)

# drop first row w/ names
data <- data[-1, ] 

# Drop incompletes
data <- subset(data, Complete == 'Good')

# Rename columns
colnames(data)[1] <- 'ID'

#---------------------------------------------------------------------------#
# List of correlations between perceptions (Q32) and Frequency of Play (Q7)

# Players only. Drop nonplayers
df <- subset(data, Quota == 'Players')

keeps <- c('ID', 'Q7', 'Q35_1', 'Q35_2', 'Q35_3', 'Q35_4', 'Q35_5', 'Q35_6', 'Q35_7', 'Q35_8', 'Q35_9',
           'Q35_10', 'Q35_11', 'Q35_12', 'Q35_13', 'Q35_14', 'Q35_15', 'Q35_16', 'Q35_17', 'Q35_18', 'Q35_19',
           'Q35_20', 'Q35_21', 'Q35_22', 'Q35_23', 'Q35_24', 'Q35_25', 'Q35_26', 'Q35_27', 'Q35_28')
df <- df[ , (names(df) %in% keeps)]

# Melt data into long format
df <- melt(df, id.vars=c('ID','Q7'), variable.name = 'Perceptions', value.name= 'Rating')

# Reset levels of Frequency
df[which(df$Q7 == 'Once a week or more '), ]$Q7 <- 5
df[which(df$Q7 == '2 to 3 times a month'), ]$Q7 <- 4
df[which(df$Q7 == 'Once a month '), ]$Q7 <- 3
df[which(df$Q7 == 'Once every 3 months'), ]$Q7 <- 2
df[which(df$Q7 == 'Once every 6 to 12 months'), ]$Q7 <- 1

# Reset levels of Ratings
df[which(df$Rating == '1 = does not describe at all'), ]$Rating <- 1
df[which(df$Rating == '2'), ]$Rating <- 2
df[which(df$Rating == '3'), ]$Rating <- 3
df[which(df$Rating == '4'), ]$Rating <- 4
df[which(df$Rating == '5 = describes completely'), ]$Rating <- 5

# Change value type of vars
df$Q7 <- as.numeric(df$Q7)
df$Rating <- as.numeric(df$Rating)

# Cast data into wide format
df <- dcast(df, ID+Q7~Perceptions)

cor(df$Q7, df$Q35_1)
cor(df$Q7, df$Q35_2)
cor(df$Q7, df$Q35_3)
cor(df$Q7, df$Q35_4)
cor(df$Q7, df$Q35_5)
cor(df$Q7, df$Q35_6)
cor(df$Q7, df$Q35_7)
cor(df$Q7, df$Q35_8)
cor(df$Q7, df$Q35_9)
cor(df$Q7, df$Q35_10)
cor(df$Q7, df$Q35_11)
cor(df$Q7, df$Q35_12)
cor(df$Q7, df$Q35_13)
cor(df$Q7, df$Q35_14)
cor(df$Q7, df$Q35_15)
cor(df$Q7, df$Q35_16)
cor(df$Q7, df$Q35_17)
cor(df$Q7, df$Q35_18)
cor(df$Q7, df$Q35_19)
cor(df$Q7, df$Q35_20)
cor(df$Q7, df$Q35_21)
cor(df$Q7, df$Q35_22)
cor(df$Q7, df$Q35_23)
cor(df$Q7, df$Q35_24)
cor(df$Q7, df$Q35_25)
cor(df$Q7, df$Q35_26)
cor(df$Q7, df$Q35_27)
cor(df$Q7, df$Q35_28)

# for(i in c('Q35_1', 'Q35_2', 'Q35_3', 'Q35_4', 'Q35_5', 'Q35_6', 'Q35_7', 'Q35_8', 'Q35_9',
#            'Q35_10', 'Q35_11', 'Q35_12', 'Q35_13', 'Q35_14', 'Q35_15', 'Q35_16', 'Q35_17', 'Q35_18', 'Q35_19',
#            'Q35_20', 'Q35_21', 'Q35_22', 'Q35_23', 'Q35_24', 'Q35_25', 'Q35_26', 'Q35_27', 'Q35_28')){
#    my_plot <- 
#      ggplot(df, aes(x = Q7, y = i)) + 
#       geom_point(position = position_jitter(w = 0.2, h = 0.2), alpha=0.2, color='firebrick', size=3) +
#       xlab("Frequency of Play") +
#       ylab(i) + theme() + geom_smooth(method='lm')
#   ggsave(paste(i,'.png',sep=''), plot = my_plot) 
# }

#---------------------------------------------------------------------------#
# List of correlations between Perceptions (Q32) and Consideration of Play of
# Powerball (Q27_01), MegaMillions (Q27_02), and Scratchoffs (Q27_06)

# Nonplayers only. Drop players
df <- subset(data, Quota == 'Non-Players')

keeps <- c('ID', 'Q35_1', 'Q35_2', 'Q35_3', 'Q35_4', 'Q35_5', 'Q35_6', 'Q35_7', 'Q35_8', 'Q35_9',
           'Q35_10', 'Q35_11', 'Q35_12', 'Q35_13', 'Q35_14', 'Q35_15', 'Q35_16', 'Q35_17', 'Q35_18', 'Q35_19',
           'Q35_20', 'Q35_21', 'Q35_22', 'Q35_23', 'Q35_24', 'Q35_25', 'Q35_26', 'Q35_27', 'Q35_28', 
           'Q27_1', 'Q27_2', 'Q27_6')
df <- df[ , (names(df) %in% keeps)]

# Melt data into long format
df <- melt(df, id.vars=c('ID', 'Q27_1', 'Q27_2', 'Q27_6'), variable.name = 'Perceptions', value.name= 'Rating')
df <- melt(df, id.vars=c('ID', 'Perceptions', 'Rating'), variable.name = 'Game', value.name= 'Consideration')

# Set levels of Consideration
df[which(df$Consideration == 'Very Unlikely'), ]$Consideration <- 1
df[which(df$Consideration == 'Somewhat Unlikely'), ]$Consideration <- 2
df[which(df$Consideration == 'Neither unlikely nor likely'), ]$Consideration <- 3
df[which(df$Consideration == 'Somewhat Likely'), ]$Consideration <- 4
df[which(df$Consideration == 'Very Likely'), ]$Consideration <- 5

# Set levels of Ratings
df[which(df$Rating == '1 = does not describe at all'), ]$Rating <- 1
df[which(df$Rating == '2'), ]$Rating <- 2
df[which(df$Rating == '3'), ]$Rating <- 3
df[which(df$Rating == '4'), ]$Rating <- 4
df[which(df$Rating == '5 = describes completely'), ]$Rating <- 5

# Change value type of vars
df$Consideration <- as.numeric(df$Consideration)
df$Rating <- as.numeric(df$Rating)

# Cast data into wide format
df <- dcast(df, ID+Perceptions+Rating~Game)

# Calculate mean of consideration scores
df$Consider <- NA
df$Consider <- rowMeans(df[, c('Q27_1','Q27_2','Q27_6')], na.rm=TRUE)

# Drop individual game columns
df <- df[ , c('ID','Perceptions','Rating','Consider')]

# Drop rows with NaNs
df <- df[complete.cases(df), ]

# Cast data into wide format 
df <- dcast(df, ID + Consider ~ Perceptions, value.var="Rating")

cor(df$Consider, df$Q35_1)
cor(df$Consider, df$Q35_2)
cor(df$Consider, df$Q35_3)
cor(df$Consider, df$Q35_4)
cor(df$Consider, df$Q35_5)
cor(df$Consider, df$Q35_6)
cor(df$Consider, df$Q35_7)
cor(df$Consider, df$Q35_8)
cor(df$Consider, df$Q35_9)
cor(df$Consider, df$Q35_10)
cor(df$Consider, df$Q35_11)
cor(df$Consider, df$Q35_12)
cor(df$Consider, df$Q35_13)
cor(df$Consider, df$Q35_14)
cor(df$Consider, df$Q35_15)
cor(df$Consider, df$Q35_16)
cor(df$Consider, df$Q35_17)
cor(df$Consider, df$Q35_18)
cor(df$Consider, df$Q35_19)
cor(df$Consider, df$Q35_20)
cor(df$Consider, df$Q35_21)
cor(df$Consider, df$Q35_22)
cor(df$Consider, df$Q35_23)
cor(df$Consider, df$Q35_24)
cor(df$Consider, df$Q35_25)
cor(df$Consider, df$Q35_26)
cor(df$Consider, df$Q35_27)
cor(df$Consider, df$Q35_28)

#---------------------------------------------------------------------------#
# Create table of Perception ratings by Relationship Type

# Survey data from Critical Mix panel
df <- read.csv("../Final Data Files/MUL1503_NCEL.csv", header = T, na.strings='NA', skip=0, stringsAsFactors=F)

# drop first row w/ names
df <- df[-1, ] 

# Drop incompletes
df <- subset(df, Complete == 'Good')

# Rename columns
colnames(df)[1] <- 'ID'

# Relationship
# Q33 Type of relationship
# Q35_10 "Provides a fun experience"
# Q35_26 "For people like me"
# Q35_16  "I feel good about playing their games"
keeps <- c('ID', 'Q33', 'Q35_25', 'Q35_10', 'Q35_16', 'Q35_19', 'Q35_24')
df <- df[ , (names(df) %in% keeps)]

# Melt data into long format
df <- melt(df, id.vars=c('ID','Q33'), variable.name = 'Perceptions', value.name= 'Rating')

# Recode levels of Ratings
df[which(df$Rating == '1 = does not describe at all'), ]$Rating <- 1
df[which(df$Rating == '2'), ]$Rating <- 2
df[which(df$Rating == '3'), ]$Rating <- 3
df[which(df$Rating == '4'), ]$Rating <- 4
df[which(df$Rating == '5 = describes completely'), ]$Rating <- 5

# Convert type
df$Rating <- as.numeric(df$Rating)

# Cast data into wide format
df <- dcast(df, ID+Q33~Perceptions)

# Subset by type of relationship
relation <- 'Casual Acquaintance '
sub <- subset(df, Q33==relation)
mean(sub$Q35_25)
mean(sub$Q35_10)
mean(sub$Q35_16)
mean(sub$Q35_19)
mean(sub$Q35_24)

relation <- 'Complete Stranger'
sub <- subset(df, Q33==relation)
mean(sub$Q35_25)
mean(sub$Q35_10)
mean(sub$Q35_16)
mean(sub$Q35_19)
mean(sub$Q35_24)

relation <- 'Buddy/Friend'
sub <- subset(df, Q33==relation)
mean(sub$Q35_25)
mean(sub$Q35_10)
mean(sub$Q35_16)
mean(sub$Q35_19)
mean(sub$Q35_24)

relation <- 'Frenemy (friend/enemy)'
sub <- subset(df, Q33==relation)
mean(sub$Q35_25)
mean(sub$Q35_10)
mean(sub$Q35_16)
mean(sub$Q35_19)
mean(sub$Q35_24)







