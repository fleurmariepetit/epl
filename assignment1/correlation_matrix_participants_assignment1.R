#Experimentation in Psychology and Linguistics assignment 1 part 2
require(ggplot2)

#Read our results from assignment 1
df <- read.csv("results.csv", header= TRUE)
df <- df[23:60,]
rownames(df) <- NULL

#Create a correlation matrix for every participant
cor_matrix <- round(cor(t(df[,5:34])),3)
cor_matrix <- as.data.frame(cor_matrix) 
cor_matrix[upper.tri(cor_matrix)] <- NA

#Calculate the total average mean of every word pair
total_score <- colSums(df[,5:34])/nrow(df)
