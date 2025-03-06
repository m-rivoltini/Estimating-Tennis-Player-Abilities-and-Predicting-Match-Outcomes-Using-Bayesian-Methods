#This script file performs the predictions for out-of-sample matches for M3 on grass surface (Grand Slam Tour)

#clearing environment and loading packages
rm(list=ls())

library(Rlab)
library(caret)
library(pROC)
library(bayesplot)

#setting working directory
setwd("~/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Predictions/gorgi_reg_nosurf/grand_slam/grand_slam_grass")

#loading data sets
atp_matches_2000_train <- read.csv2(file = "atp_matches_2000_train_GS_grass.csv",header = TRUE)
atp_players_2000_train <- read.csv2(file = "atp_players_2000_train_GS_grass.csv",header = TRUE)

#Note that there are more players when considering the test set, so we have to exclude those matches which include a player not in the training set.
#Moreover, since there are other players which are added, we need to be careful of the player IDs which are now different
atp_matches_2000_train_test <- read.csv2(file = "atp_matches_2000_train_test_GS_grass.csv",header = TRUE)
atp_players_2000_train_test <- read.csv2(file = "atp_players_2000_train_test_GS_grass.csv",header = TRUE)

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

#redefine the time-units to be sequential (we are assuming these are the matches we have to begin with)
atp_matches_2000_test$Time <- (atp_matches_2000_train$Time[length(atp_matches_2000_train$Time)]+1):(atp_matches_2000_train$Time[length(atp_matches_2000_train$Time)]+nrow(atp_matches_2000_test))


############################################################################################################################
#Proceed to perform the predictions on the test set
############################################################################################################################

#loading the outputs
load("/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/gorgi_reg_nosurf/grand_slam/grand_slam_grass/gorgi_reg_nosurf_GS_grass_with_extra_burnin_stand_pred_noht_noage.RData")

#Using the latest player abilities only
lambda <-out$sims.list$lambda[,,nrow(atp_matches_2000_train)] 

tau <- out$sims.list$tau
p <- out$sims.list$p_ij[,nrow(atp_matches_2000_train)]

beta_B <-out$sims.list$beta_B
beta_R <-out$sims.list$beta_R
#creating a vector to store posterior samples for predicted win probabilities
p_post_preds <- mat.or.vec(nrow(lambda),nrow(atp_matches_2000_test)) #each column represents a sample of 3000 for the predicted win probability for each match 

########First Approach: Use only the most up to date strength estimates to predict ALL the matches##########

#This will store the final decision as to whether i_t beat j_t using only the abilities obtained at the last time-point
y_pred_entire <- mat.or.vec(nrow(atp_matches_2000_test),1) #uses median
y_pred_entire_2 <- mat.or.vec(nrow(atp_matches_2000_test),1) #samples from bernoulli and counts number of ones and zeros

#This will sample from a bernoulli random variable for each posterior probability in the posterior sample
Y_post_pred <- mat.or.vec(nrow(lambda),nrow(atp_matches_2000_test))

for (t in 1:nrow(atp_matches_2000_test)){ #working the logit function to get posterior predicted probabilities
  p_post_preds[,t] <- exp(lambda[,atp_matches_2000_test$player_i_id[t]]+beta_R*(atp_matches_2000_test$transformed_rank_i[t] - mean(atp_matches_2000_train$transformed_rank_i))/sqrt(var(atp_matches_2000_train$transformed_rank_i) + var(atp_matches_2000_train$transformed_rank_j)) + beta_B*(atp_matches_2000_test$bp_saved_ratio_i[t] - mean(atp_matches_2000_train$bp_saved_ratio_i))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_i)+var(atp_matches_2000_train$bp_saved_ratio_j)))/(exp(lambda[,atp_matches_2000_test$player_i_id[t]]+beta_R*(atp_matches_2000_test$transformed_rank_i[t] - mean(atp_matches_2000_train$transformed_rank_i))/sqrt(var(atp_matches_2000_train$transformed_rank_i) + var(atp_matches_2000_train$transformed_rank_j)) + beta_B*(atp_matches_2000_test$bp_saved_ratio_i[t] - mean(atp_matches_2000_train$bp_saved_ratio_i))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_i)+var(atp_matches_2000_train$bp_saved_ratio_j)))+exp(lambda[,atp_matches_2000_test$player_j_id[t]]+beta_R*(atp_matches_2000_test$transformed_rank_j[t] - mean(atp_matches_2000_train$transformed_rank_j))/sqrt(var(atp_matches_2000_train$transformed_rank_i) + var(atp_matches_2000_train$transformed_rank_j)) + beta_B*(atp_matches_2000_test$bp_saved_ratio_j[t] - mean(atp_matches_2000_train$bp_saved_ratio_j))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_i)+var(atp_matches_2000_train$bp_saved_ratio_j))))
}

for (t in 1:nrow(atp_matches_2000_test)){
  Y_post_pred[,t] <- rbern(nrow(p_post_preds),p_post_preds[,t]) #obtaining a sample from the posterior predictive distribution
}

#Either look at the median of p_post_preds and if >= 0.5 set y_pred=1, otherwise use counts of Y_pred

#first option
for (t in 1:nrow(atp_matches_2000_test)){
  if (median(p_post_preds[,t])>=0.5){
    y_pred_entire[t] <- 1
  }
  else{
    y_pred_entire[t] <- 0
  }
}

#second option
for (t in 1:nrow(atp_matches_2000_test)){
  if (sum(Y_post_pred[,t]==1)>=sum(Y_post_pred[,t]==0)){
    y_pred_entire_2[t] <- 1
  }
  else{
    y_pred_entire_2[t] <- 0
  }
}

#ensuring variables are factors for confusionMatrix() command
#y_pred <- as.factor(y_pred)
#y_pred_2 <- as.factor(y_pred_2)
atp_matches_2000_test$y <- as.factor(atp_matches_2000_test$y)

#adding the predictions as a column to the data
atp_matches_2000_test$y_pred_entire <- as.factor(y_pred_entire)
atp_matches_2000_test$y_pred_entire_2 <- as.factor(y_pred_entire_2)

#generating the confusion matrix - both approaches give identical results...
confusionMatrix(atp_matches_2000_test$y_pred_entire,reference = atp_matches_2000_test$y)
confusionMatrix(atp_matches_2000_test$y_pred_entire_2,reference = atp_matches_2000_test$y)

#Plotting the ROC curve
roc_curve <- roc(response = atp_matches_2000_test$y_pred_entire,predictor = as.numeric(atp_matches_2000_test$y))
plot(roc_curve)

######## Expanding Window: After every match, update the strengths of the respective players ##########


#creating a vector to store posterior samples for predicted win probabilities
p_post_preds_exp_win <- mat.or.vec(nrow(lambda),nrow(atp_matches_2000_test)) #each column represents a sample of 3000 for the predicted win probability for each match 

y_pred_exp_win <- mat.or.vec(nrow(atp_matches_2000_test),1) #uses median
y_pred_exp_win_2 <- mat.or.vec(nrow(atp_matches_2000_test),1)

lambda_new <- mat.or.vec(nrow(lambda),nrow(atp_players_2000_train)) #This will store the matrix of abilities (rows correspond to posterior samples and columns to players) at the subsequent time-point

lambda_pred <-array(NaN, c(nrow(lambda), nrow(atp_players_2000_train), nrow(atp_matches_2000_test))) #This will store the set of all updated strengths

Y_post_pred_exp_win <- mat.or.vec(nrow(lambda),nrow(atp_matches_2000_test))


#Doing the first iteration on its own since it uses the last entry of the training set to predict the first entry of the test set.

s_i <- atp_matches_2000_train$y[nrow(atp_matches_2000_train)]*(1-p) - (1-atp_matches_2000_train$y[nrow(atp_matches_2000_train)])*p
s_j <- -s_i

for (i in 1:nrow(atp_players_2000_train)){
  if (i == atp_matches_2000_train$player_i_id[nrow(atp_matches_2000_train)]){
    lambda_new[,i] = lambda[,i] + tau*s_i
  }
  if (i == atp_matches_2000_train$player_j_id[nrow(atp_matches_2000_train)]){
    lambda_new[,i] = lambda[,i] + tau*s_j
  }
  if (i !=atp_matches_2000_train$player_j_id[nrow(atp_matches_2000_train)] & i != atp_matches_2000_train$player_i_id[nrow(atp_matches_2000_train)]){
    lambda_new[,i] = lambda[,i]
  }
}

lambda_pred[,,1] = lambda_new

p_post_preds_exp_win[,1] <- exp(lambda_new[,atp_matches_2000_test$player_i_id[1]] + beta_R*(atp_matches_2000_test$transformed_rank_i[1] - mean(atp_matches_2000_train$transformed_rank_i))/sqrt(var(atp_matches_2000_train$transformed_rank_i)+var(atp_matches_2000_train$transformed_rank_j)) + beta_B*(atp_matches_2000_test$bp_saved_ratio_i[1] - mean(atp_matches_2000_train$bp_saved_ratio_i))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_i)+var(atp_matches_2000_train$bp_saved_ratio_j)))/(exp(lambda_new[,atp_matches_2000_test$player_i_id[1]]+ beta_R*(atp_matches_2000_test$transformed_rank_i[1] - mean(atp_matches_2000_train$transformed_rank_i))/sqrt(var(atp_matches_2000_train$transformed_rank_i)+var(atp_matches_2000_train$transformed_rank_j)) + beta_B*(atp_matches_2000_test$bp_saved_ratio_i[1] - mean(atp_matches_2000_train$bp_saved_ratio_i))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_i)+var(atp_matches_2000_train$bp_saved_ratio_j)))+exp(lambda_new[,atp_matches_2000_test$player_j_id[1]]+ beta_R*(atp_matches_2000_test$transformed_rank_j[1] - mean(atp_matches_2000_train$transformed_rank_j))/sqrt(var(atp_matches_2000_train$transformed_rank_j)+var(atp_matches_2000_train$transformed_rank_i)) + beta_B*(atp_matches_2000_test$bp_saved_ratio_j[1] - mean(atp_matches_2000_train$bp_saved_ratio_j))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_j)+var(atp_matches_2000_train$bp_saved_ratio_i))))


#Doing the remaining of the predictions

for (t in 2:(nrow(atp_matches_2000_test))){
  s_i <- (as.numeric(atp_matches_2000_test$y[t-1])-1)*(1-p_post_preds_exp_win[,t-1]) - (1-(as.numeric(atp_matches_2000_test$y[t-1])-1))*p_post_preds_exp_win[,t-1]
  s_j <- -s_i
  for (i in 1:nrow(atp_players_2000_train)){
    if (i == atp_matches_2000_test$player_i_id[t-1]){
      lambda_pred[,i,t] = lambda_pred[,i,t-1] + tau*s_i
    }
    if (i == atp_matches_2000_test$player_j_id[t-1]){
      lambda_pred[,i,t] = lambda_pred[,i,t-1] + tau*s_j
    }
    if (i != atp_matches_2000_test$player_i_id[t-1] && i != atp_matches_2000_test$player_j_id[t-1]){
      lambda_pred[,i,t] = lambda_pred[,i,t-1]
    }
  }
  p_post_preds_exp_win[,t] <- exp(lambda_pred[,atp_matches_2000_test$player_i_id[t],t]+ beta_R*(atp_matches_2000_test$transformed_rank_i[t] - mean(atp_matches_2000_train$transformed_rank_i))/sqrt(var(atp_matches_2000_train$transformed_rank_i)+var(atp_matches_2000_train$transformed_rank_j)) + beta_B*(atp_matches_2000_test$bp_saved_ratio_i[t] - mean(atp_matches_2000_train$bp_saved_ratio_i))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_i)+var(atp_matches_2000_train$bp_saved_ratio_j)))/(exp(lambda_pred[,atp_matches_2000_test$player_i_id[t],t]+ beta_R*(atp_matches_2000_test$transformed_rank_i[t] - mean(atp_matches_2000_train$transformed_rank_i))/sqrt(var(atp_matches_2000_train$transformed_rank_i)+var(atp_matches_2000_train$transformed_rank_j)) + beta_B*(atp_matches_2000_test$bp_saved_ratio_i[t] - mean(atp_matches_2000_train$bp_saved_ratio_i))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_i)+var(atp_matches_2000_train$bp_saved_ratio_j)))+exp(lambda_pred[,atp_matches_2000_test$player_j_id[t],t]+ beta_R*(atp_matches_2000_test$transformed_rank_j[t] - mean(atp_matches_2000_train$transformed_rank_j))/sqrt(var(atp_matches_2000_train$transformed_rank_j)+var(atp_matches_2000_train$transformed_rank_i)) + beta_B*(atp_matches_2000_test$bp_saved_ratio_j[t] - mean(atp_matches_2000_train$bp_saved_ratio_j))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_j)+var(atp_matches_2000_train$bp_saved_ratio_i))))
}

#using the first classification approach
for (t in 1:nrow(atp_matches_2000_test)){
  if (median(p_post_preds_exp_win[,t])>=0.5){
    y_pred_exp_win[t] = 1  
  }else{
    y_pred_exp_win[t] = 0
  }
}
#Using the second approach classification approach
for (t in 1:nrow(atp_matches_2000_test)){
  Y_post_pred_exp_win[,t] <- rbern(nrow(p_post_preds_exp_win),p_post_preds_exp_win[,t]) 
}
for (t in 1:nrow(atp_matches_2000_test)){
  if (sum(Y_post_pred_exp_win[,t]==1)>=sum(Y_post_pred_exp_win[,t]==0)){
    y_pred_exp_win_2[t] <- 1
  }else{
    y_pred_exp_win_2[t] <- 0
  }
}

#adding the predictions as a column to the data
atp_matches_2000_test$y_pred_exp_win <- as.factor(y_pred_entire)
atp_matches_2000_test$y_pred_exp_win_2 <- as.factor(y_pred_entire_2)

#generating the confusion matrix - both approaches give identical results...
confusionMatrix(atp_matches_2000_test$y_pred_exp_win,reference = atp_matches_2000_test$y)
confusionMatrix(atp_matches_2000_test$y_pred_exp_win_2,reference = atp_matches_2000_test$y)

#Plotting the ROC curve
roc_curve <- roc(response = atp_matches_2000_test$y_pred_exp_win,predictor = as.numeric(atp_matches_2000_test$y)-1)
plot(roc_curve)

roc_curve <- roc(response = atp_matches_2000_test$y_pred_exp_win_2,predictor = as.numeric(atp_matches_2000_test$y)-1)
plot(roc_curve)



