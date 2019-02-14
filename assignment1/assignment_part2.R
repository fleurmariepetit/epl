#Experimentation in Psychology and Linguistics assignment 1 part 2


#################Questions####################
#Compare responses to two groups of stimuli
#Did participants tend to agree with each other?
#Are the 2 groups of stimuli homogenous?
#Do they contrast strongly?
##############################################

require(ggplot2)
data <- read.csv("raw_data_assignment1.csv", header= TRUE)

#Create a vector which contains the indexes of similar word pairs
sim_index <- c(2,3,9,10,14,16:22,24,27,29)

#Create 2 seperate datasets of similar and dissimilar word pairs
similar_data <- data[,sim_index]
dissimilar_data <- data[,-(sim_index)]

#apply statistical methods
sim_analysis <- as.data.frame(apply(similar_data,2, mean)) #Caluclate the mean of results
names(sim_analysis) <- c("mean")
sim_analysis$sd <- apply(similar_data,2, sd) #Caluclate the sd of results
total_sim <- c(mean(sim_analysis$mean), mean(sim_analysis$sd)) #Caluclate the average mean and sd of results

dissim_analysis <- as.data.frame(apply(dissimilar_data,2, mean)) #Caluclate the mean of results
names(dissim_analysis) <- c("mean")
dissim_analysis$sd <- apply(dissimilar_data,2, sd) #Caluclate the sd of results
total_dissim <- c(mean(dissim_analysis$mean), mean(dissim_analysis$sd)) #Caluclate the average mean and sd of results

#Check whether participants correlate in responses
cor_sim_matrix <- as.data.frame(round(cor(t(similar_data)),3))
cor_dissim_matrix <- as.data.frame(round(cor(t(dissimilar_data)),3))

#Check whether group significantly differ
cor.test(colMeans(similar_data), colMeans(dissimilar_data)) #low correlation
t.test(similar_data, dissimilar_data) # low p-value

##########################################################################################################################################################
################################################## Lab Session 2 practicals ##############################################################################
##########################################################################################################################################################

#Load response data of first lab session with too many word pairs
class_data <- read.csv("Similarity_judgements_INFOMEPL - Form Responses 1.csv",header = TRUE)

#Pick random participant data and make a histogram out of it
respondent20 <- t(class_data[20,2:178])
hist(respondent20)

respondent10 <- t(class_data[10,2:178])
hist(respondent10)

respondent6 <- t(class_data[6,2:178])
hist(respondent6)

#Random respondence, not correlated to other students, Histogram is very weird. Clearly outlier data.
respondent1 <- t(class_data[1,2:178])
hist(respondent1)

#Create a histogram of all the responses
hist(t(class_data[,2:178]), col = "red")

t.test(respondent20,respondent11)
plot(respondent20, respondent11)
cor.test(respondent6,respondent10)

#Create a correlation matrix of all participants
cor_matrix <- round(cor(t(class_data[,2:178])),3)
cor_matrix <- as.data.frame(cor_matrix)

#Load the wordSim sample given by Denis boi
wsdata <- read.csv("wordsim_sample.csv", header = TRUE)

#Does correlate
cor.test(respondent20, t(wsdata))

#Does not correlate
cor.test(respondent1, t(wsdata))

#Write up follow-up study based on the results of your own gathered data
#Does correlate with average participant
cor.test(colMeans(class_data[,2:178]), t(wsdata))

######################################################################################################################################
######################################################################################################################################

