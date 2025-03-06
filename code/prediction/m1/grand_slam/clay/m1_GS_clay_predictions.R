#This script file performs the predictions for out-of-sample matches for M1 on clay (Grand Slam Tour)

#clearing environment and loading packages
rm(list=ls())

library(Rlab)
library(caret)
library(pROC)

#setting working directory
setwd("~/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Predictions/fixed_time_bt/grand_slam/grand_slam_clay")

#loading data sets
atp_matches_2000_train <- read.csv2(file = "atp_matches_2000_train_GS_clay.csv",header = TRUE)
atp_players_2000_train <- read.csv2(file = "atp_players_2000_train_GS_clay.csv",header = TRUE)

#Note that there are more players when considering the test set, so we have to exclude those matches which include a player not in the training set.
#Moreover, since there are other players which are added, we need to be careful of the player IDs which are now different
atp_matches_2000_train_test <- read.csv2(file = "atp_matches_2000_train_test_GS_clay.csv",header = TRUE)
atp_players_2000_train_test <- read.csv2(file = "atp_players_2000_train_test_GS_clay.csv",header = TRUE)

############################################################################################################################
#Cleaning the test set
############################################################################################################################

#removing matches in which one player is not in the training set
train_players <- unique(c(atp_matches_2000_train$winner_name,atp_matches_2000_train$loser_name)) #find all players in the training set
train_test_players <- unique(c(atp_matches_2000_train_test$winner_name,atp_matches_2000_train_test$loser_name)) #find all players in the training and test sets combined
players_to_remove <- train_test_players[which(!(train_test_players %in% train_players))] #list of players which are in the test set but not in the training set

#removing these players
atp_matches_2000_train_test <- atp_matches_2000_train_test[which(!(atp_matches_2000_train_test$winner_name %in% players_to_remove) & !(atp_matches_2000_train_test$loser_name %in% players_to_remove)),]

#checking that the number of players are the same
length(unique(c(atp_matches_2000_train_test$winner_name,atp_matches_2000_train_test$loser_name)))

#taking the subset of the training set.
atp_matches_2000_test <- atp_matches_2000_train_test[which(atp_matches_2000_train_test$year %in% c(2023,2024)),]

#checks

length(unique(c(atp_matches_2000_test$winner_name,atp_matches_2000_test$loser_name)))
#Since the number of players were different, the player IDs have changed. So we match the IDs in the test set with those of the training set.
atp_matches_2000_test$winner_id <- atp_players_2000_train$player_id[match(unlist(atp_matches_2000_test$winner_name),atp_players_2000_train$name)]

atp_matches_2000_test$loser_id <- atp_players_2000_train$player_id[match(unlist(atp_matches_2000_test$loser_name),atp_players_2000_train$name)]

atp_matches_2000_test$player_i_id <- atp_players_2000_train$player_id[match(unlist(atp_matches_2000_test$player_i_name),atp_players_2000_train$name)]

atp_matches_2000_test$player_j_id <- atp_players_2000_train$player_id[match(unlist(atp_matches_2000_test$player_j_name),atp_players_2000_train$name)]

#checks
length(unique(c(atp_matches_2000_test$winner_name,atp_matches_2000_test$loser_name)))

############################################################################################################################
#Proceed to perform the predictions on the test set
############################################################################################################################

#loading the outputs of the BT_GS_clay
load("/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/fixed_time_bt/grand_slam/grand_slam_clay/BT_fixed_time_out_GS_clay_with_extra_burnin_6.RData")

#saving the player abilities
lambda <-out$sims.list$lambda 

#creating a vector to store posterior samples for predicted win probabilities
p_post_preds <- mat.or.vec(nrow(lambda),nrow(atp_matches_2000_test)) #each column represents a sample of 15000 for the predicted win probability for each match 

#This will sample from a bernoulli random variable for each posterior probability in the posterior sample
Y_post_pred <- mat.or.vec(nrow(lambda),nrow(atp_matches_2000_test))

#This will store the final decision as to whether i_t beat j_t
y_pred <- mat.or.vec(nrow(atp_matches_2000_test),1)
y_pred_2 <- mat.or.vec(nrow(atp_matches_2000_test),1)

for (t in 1:nrow(atp_matches_2000_test)){
  p_post_preds[,t] <- exp(lambda[,atp_matches_2000_test$player_i_id[t]])/(exp(lambda[,atp_matches_2000_test$player_i_id[t]])+exp(lambda[,atp_matches_2000_test$player_j_id[t]]))
}

for (t in 1:nrow(atp_matches_2000_test)){
  Y_post_pred[,t] <- rbern(nrow(p_post_preds),p_post_preds[,t]) #obtaining a sample from the posterior predictive distribution
}

#Either look at the median of p_post_preds and if >= 0.5 set y_pred=1, otherwise use counts of Y_pred

#first option
for (t in 1:nrow(atp_matches_2000_test)){
  if (median(p_post_preds[,t])>=0.5){
    y_pred[t] <- 1
  }
  else{
    y_pred[t] <- 0
  }
}

#second option
for (t in 1:nrow(atp_matches_2000_test)){
  if (sum(Y_post_pred[,t]==1)>=sum(Y_post_pred[,t]==0)){
    y_pred_2[t] <- 1
  }
  else{
    y_pred_2[t] <- 0
  }
}

#ensuring variables are factors for confusionMatrix() command
y_pred <- as.factor(y_pred)
y_pred_2 <- as.factor(y_pred_2)
atp_matches_2000_test$y <- as.factor(atp_matches_2000_test$y)

#adding the predictions as a column to the data
atp_matches_2000_test$y_pred <- y_pred
atp_matches_2000_test$y_pred_2 <- y_pred_2

#generating the confusion matrix - both approaches give identical results...
confusionMatrix(atp_matches_2000_test$y_pred,reference = atp_matches_2000_test$y)
confusionMatrix(atp_matches_2000_test$y_pred_2,reference = atp_matches_2000_test$y)

#plotting ROC curve
roc_curve <- roc(response = atp_matches_2000_test$y_pred,predictor = as.numeric(atp_matches_2000_test$y))
plot(roc_curve)
