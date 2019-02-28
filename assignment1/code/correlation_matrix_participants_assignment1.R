#Experimentation in Psychology and Linguistics assignment 1 part 2
library(ggplot2)
library(tidyverse)

#Read our results from assignment 1
df <- read.csv("sheet1.csv", header= TRUE)
df <- df[23:60,]
rownames(df) <- NULL

#Count the amount of responses per score
response <- t(df[,5:34]) %>% as.data.frame()

#plot histogram plots for each participant
ggplot(data=response, aes(x=factor(response$V17))) + 
  geom_bar(stat = "count") + labs(title="histogram of scores for participant 17") + geom_text(stat='count',aes(label=..count..),vjust=-1) +
  xlab("Participant 17")

#Create a correlation matrix for every participant
cor_matrix <- round(cor(t(df[,5:34])),3)
cor_matrix <- as.data.frame(cor_matrix) 
cor_matrix[upper.tri(cor_matrix)] <- NA

#Calculate the total average mean of every word pair
total_score <- colSums(df[,5:34])/nrow(df)

#Seperate similar and dissimilar word pairs
score_df <- df[,5:34]
sim_index <- c(2,5,6,8,9,10,16,18,19,20,22,23,26,29,30)
similar_words <- score_df[,sim_index]
dissimalar_words <- score_df[,-(sim_index)]

#Bekijk correlatie/scores tussen groepen alpha, beta en gamma?
#Bekijk correlatie/scores tussen wo, hbo en work?
#Bekijk algemeen wat de gemiddelde scores zijn?
#Gebruik t.test om te kijken of er daadwerkelijk verschil is. 