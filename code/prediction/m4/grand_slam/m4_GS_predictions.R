#This script file performs the predictions for out-of-sample matches for M3 on hard surface (Masters Tour)

#clearing environment and loading packages
rm(list=ls())

library(Rlab)
library(caret)
library(pROC)
library(bayesplot)

#setting working directory
setwd("~/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Predictions/gorgi_reg_surf/grand_slam")

#loading data sets
atp_matches_2000_train <- read.csv2(file = "atp_matches_2000_train_GS_allsurf_reg_surf_reduced.csv",header = TRUE)
atp_players_2000_train <- read.csv2(file = "atp_players_2000_train_GS_allsurf_reg_surf_reduced.csv",header = TRUE)

#Note that there are more players when considering the test set, so we have to exclude those matches which include a player not in the training set.
#Moreover, since there are other players which are added, we need to be careful of the player IDs which are now different
atp_matches_2000_train_test <- read.csv2(file = "atp_matches_2000_train_test_GS_allsurf_reg_surf.csv",header = TRUE)
atp_players_2000_train_test <- read.csv2(file = "atp_players_2000_train_test_GS_allsurf_reg_surf.csv",header = TRUE)

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

#checks (# of players in test must be less than # of players in train)

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
load("/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/gorgi_reg_surf/grand_slam/gorgi_model_reg_surf_GS_reduced_output_more_burnin.RData")

#Using the latest player abilities only.. loading outputs
lambda_b <-out$sims.list$lambda_b[,,dim(out$sims.list$lambda_b)[3]]
lambda_c <-out$sims.list$lambda_c[,,dim(out$sims.list$lambda_c)[3]]
lambda_g <-out$sims.list$lambda_g[,,dim(out$sims.list$lambda_g)[3]]
lambda_h <-out$sims.list$lambda_h[,,dim(out$sims.list$lambda_h)[3]]

tau_b <- out$sims.list$tau_b
tau_c <- out$sims.list$tau_h
tau_g <- out$sims.list$tau_g
tau_h <- out$sims.list$tau_c

p <- out$sims.list$p_ij[,dim(out$sims.list$p_ij)[2]]

beta_B <-out$sims.list$beta_B
beta_R <-out$sims.list$beta_R
#creating a vector to store posterior samples for predicted win probabilities
p_post_preds <- mat.or.vec(nrow(lambda_b),nrow(atp_matches_2000_test)) #each column represents a sample of 3000 for the predicted win probability for each match 

########First Approach: Use only the most up to date strength estimates to predict ALL the matches##########

#This will store the final decision as to whether i_t beat j_t using only the abilities obtained at the last time-point
y_pred_entire <- mat.or.vec(nrow(atp_matches_2000_test),1) #uses median
y_pred_entire_2 <- mat.or.vec(nrow(atp_matches_2000_test),1) #samples from bernoulli and counts number of ones and zeros

#This will sample from a bernoulli random variable for each posterior probability in the posterior sample
Y_post_pred <- mat.or.vec(nrow(lambda_b),nrow(atp_matches_2000_test))

for (t in 1:nrow(atp_matches_2000_test)){ #working the logit function to get posterior predicted probabilities
  lambda_i_temp <- lambda_b[,atp_matches_2000_test$player_i_id[t]] + atp_matches_2000_test$I_clay[t]*lambda_c[,atp_matches_2000_test$player_i_id[t]]+ atp_matches_2000_test$I_grass[t]*lambda_g[,atp_matches_2000_test$player_i_id[t]]+ atp_matches_2000_test$I_hard[t]*lambda_h[,atp_matches_2000_test$player_i_id[t]]
  
  lambda_j_temp <- lambda_b[,atp_matches_2000_test$player_j_id[t]] + atp_matches_2000_test$I_clay[t]*lambda_c[,atp_matches_2000_test$player_j_id[t]]+ atp_matches_2000_test$I_grass[t]*lambda_g[,atp_matches_2000_test$player_j_id[t]]+ atp_matches_2000_test$I_hard[t]*lambda_h[,atp_matches_2000_test$player_j_id[t]]
  
  
  p_post_preds[,t] <- exp(lambda_i_temp+beta_R*(atp_matches_2000_test$transformed_rank_i[t] - mean(atp_matches_2000_train$transformed_rank_i))/sqrt(var(atp_matches_2000_train$transformed_rank_i)+var(atp_matches_2000_train$transformed_rank_j)) + beta_B*(atp_matches_2000_test$bp_saved_ratio_i[t] - mean(atp_matches_2000_train$bp_saved_ratio_i))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_i)+var(atp_matches_2000_train$bp_saved_ratio_j)))/(exp(lambda_i_temp + beta_R*(atp_matches_2000_test$transformed_rank_i[t] - mean(atp_matches_2000_train$transformed_rank_i))/sqrt(var(atp_matches_2000_train$transformed_rank_i)+var(atp_matches_2000_train$transformed_rank_j)) + beta_B*(atp_matches_2000_test$bp_saved_ratio_i[t] - mean(atp_matches_2000_train$bp_saved_ratio_i))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_i)+var(atp_matches_2000_train$bp_saved_ratio_j)))+exp(lambda_j_temp + beta_R*(atp_matches_2000_test$transformed_rank_j[t] - mean(atp_matches_2000_train$transformed_rank_j))/sqrt(var(atp_matches_2000_train$transformed_rank_i)+var(atp_matches_2000_train$transformed_rank_j)) + beta_B*(atp_matches_2000_test$bp_saved_ratio_j[t] - mean(atp_matches_2000_train$bp_saved_ratio_j))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_i)+var(atp_matches_2000_train$bp_saved_ratio_j))))
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

#Plotting ROC curves
roc_curve <- roc(response = atp_matches_2000_test$y_pred_entire,predictor = as.numeric(atp_matches_2000_test$y))
plot(roc_curve)

######## Expanding Window: After every match, update the strengths of the respective players ##########


#creating a vector to store posterior samples for predicted win probabilities
p_post_preds_exp_win <- mat.or.vec(nrow(lambda_b),nrow(atp_matches_2000_test)) #each column represents a sample of 3000 for the predicted win probability for each match 

y_pred_exp_win <- mat.or.vec(nrow(atp_matches_2000_test),1) #uses median
y_pred_exp_win_2 <- mat.or.vec(nrow(atp_matches_2000_test),1)

#This will store the matrix of abilities (rows correspond to posterior samples and columns to players) at the subsequent time-point
lambda_b_new <- mat.or.vec(nrow(lambda_b),nrow(atp_players_2000_train)) 
lambda_c_new <- mat.or.vec(nrow(lambda_c),nrow(atp_players_2000_train))
lambda_g_new <- mat.or.vec(nrow(lambda_g),nrow(atp_players_2000_train))
lambda_h_new <- mat.or.vec(nrow(lambda_h),nrow(atp_players_2000_train))


#These will store the set of all updated strengths
lambda_b_pred <-array(NaN, c(nrow(lambda_b), nrow(atp_players_2000_train), nrow(atp_matches_2000_test))) 
lambda_c_pred <-array(NaN, c(nrow(lambda_c), nrow(atp_players_2000_train), nrow(atp_matches_2000_test))) 
lambda_g_pred <-array(NaN, c(nrow(lambda_g), nrow(atp_players_2000_train), nrow(atp_matches_2000_test))) 
lambda_h_pred <-array(NaN, c(nrow(lambda_h), nrow(atp_players_2000_train), nrow(atp_matches_2000_test))) 

Y_post_pred_exp_win <- mat.or.vec(nrow(lambda_b),nrow(atp_matches_2000_test))


#Doing the first iteration on its own since it uses the last entry of the training set to predict the first entry of the test set.

s_i <- atp_matches_2000_train$y[nrow(atp_matches_2000_train)]*(1-p) - (1-atp_matches_2000_train$y[nrow(atp_matches_2000_train)])*p
s_j <- -s_i

for (i in 1:nrow(atp_players_2000_train)){
  if (i == atp_matches_2000_train$player_i_id[nrow(atp_matches_2000_train)]){
    lambda_b_new[,i] = lambda_b[,i] + tau_b*s_i
    lambda_c_new[,i] = lambda_c[,i] + tau_c*atp_matches_2000_train$I_clay[nrow(atp_matches_2000_train)]*s_i
    lambda_g_new[,i] = lambda_g[,i] + tau_g*atp_matches_2000_train$I_grass[nrow(atp_matches_2000_train)]*s_i
    lambda_h_new[,i] = lambda_h[,i] + tau_h*atp_matches_2000_train$I_hard[nrow(atp_matches_2000_train)]*s_i
  }
  if (i == atp_matches_2000_train$player_j_id[nrow(atp_matches_2000_train)]){
    lambda_b_new[,i] = lambda_b[,i] + tau_b*s_j
    lambda_c_new[,i] = lambda_c[,i] + tau_c*atp_matches_2000_train$I_clay[nrow(atp_matches_2000_train)]*s_j
    lambda_g_new[,i] = lambda_g[,i] + tau_g*atp_matches_2000_train$I_grass[nrow(atp_matches_2000_train)]*s_j
    lambda_h_new[,i] = lambda_h[,i] + tau_h*atp_matches_2000_train$I_hard[nrow(atp_matches_2000_train)]*s_j
  }
  if (i !=atp_matches_2000_train$player_j_id[nrow(atp_matches_2000_train)] & i != atp_matches_2000_train$player_i_id[nrow(atp_matches_2000_train)]){
    lambda_b_new[,i] = lambda_b[,i] 
    lambda_c_new[,i] = lambda_c[,i] 
    lambda_g_new[,i] = lambda_g[,i] 
    lambda_h_new[,i] = lambda_h[,i] 
  }
}

lambda_b_pred[,,1] = lambda_b_new
lambda_c_pred[,,1] = lambda_c_new
lambda_g_pred[,,1] = lambda_g_new
lambda_h_pred[,,1] = lambda_h_new

lambda_new_i_temp <- lambda_b_new[,atp_matches_2000_test$player_i_id[1]] + atp_matches_2000_test$I_clay[1]*lambda_c_new[,atp_matches_2000_test$player_i_id[1]]+ atp_matches_2000_test$I_grass[1]*lambda_g_new[,atp_matches_2000_test$player_i_id[1]]+ atp_matches_2000_test$I_hard[1]*lambda_h_new[,atp_matches_2000_test$player_i_id[1]]

lambda_new_j_temp <- lambda_b_new[,atp_matches_2000_test$player_j_id[1]] + atp_matches_2000_test$I_clay[1]*lambda_c_new[,atp_matches_2000_test$player_j_id[1]]+ atp_matches_2000_test$I_grass[1]*lambda_g_new[,atp_matches_2000_test$player_j_id[1]]+ atp_matches_2000_test$I_hard[1]*lambda_h_new[,atp_matches_2000_test$player_j_id[1]]


p_post_preds_exp_win[,1] <- exp(lambda_new_i_temp + beta_R*(atp_matches_2000_test$transformed_rank_i[1] - mean(atp_matches_2000_train$transformed_rank_i))/sqrt(var(atp_matches_2000_train$transformed_rank_i)+var(atp_matches_2000_train$transformed_rank_j)) + beta_B*(atp_matches_2000_test$bp_saved_ratio_i[1] - mean(atp_matches_2000_train$bp_saved_ratio_i))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_i)+var(atp_matches_2000_train$bp_saved_ratio_j)))/(exp(lambda_new_i_temp+ beta_R*(atp_matches_2000_test$transformed_rank_i[1] - mean(atp_matches_2000_train$transformed_rank_i))/sqrt(var(atp_matches_2000_train$transformed_rank_i)+var(atp_matches_2000_train$transformed_rank_j)) + beta_B*(atp_matches_2000_test$bp_saved_ratio_i[1] - mean(atp_matches_2000_train$bp_saved_ratio_i))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_i)+var(atp_matches_2000_train$bp_saved_ratio_j)))+exp(lambda_new_j_temp+ beta_R*(atp_matches_2000_test$transformed_rank_j[1] - mean(atp_matches_2000_train$transformed_rank_j))/sqrt(var(atp_matches_2000_train$transformed_rank_j)+var(atp_matches_2000_train$transformed_rank_i)) + beta_B*(atp_matches_2000_test$bp_saved_ratio_j[1] - mean(atp_matches_2000_train$bp_saved_ratio_j))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_j)+var(atp_matches_2000_train$bp_saved_ratio_i))))


#Doing the remaining of the predictions

#Looping for each match in the test set
for (t in 2:(nrow(atp_matches_2000_test))){
  #calculating the score for each match in the test set... used to update the strength from t-1 to t
  s_i <- (as.numeric(atp_matches_2000_test$y[t-1])-1)*(1-p_post_preds_exp_win[,t-1]) - (1-(as.numeric(atp_matches_2000_test$y[t-1])-1))*p_post_preds_exp_win[,t-1]
  s_j <- -s_i
  #Looping for each player in the whole data set (i.e training set), so that we can update all their abilities, and not just the ones who are in the test set
  for (i in 1:nrow(atp_players_2000_train)){
    #if player i played at time t-1 (and is player i_t), then update his strength to time t using tau_s and s_i
    if (i == atp_matches_2000_test$player_i_id[t-1]){
      lambda_b_pred[,i,t] = lambda_b_pred[,i,t-1] + tau_b*s_i
      lambda_c_pred[,i,t] = lambda_c_pred[,i,t-1] + tau_c*s_i*atp_matches_2000_test$I_clay[t-1]
      lambda_g_pred[,i,t] = lambda_g_pred[,i,t-1] + tau_g*s_i*atp_matches_2000_test$I_grass[t-1]
      lambda_h_pred[,i,t] = lambda_h_pred[,i,t-1] + tau_h*s_i*atp_matches_2000_test$I_hard[t-1]
      
    }
    #Same procedure, but if player i is now j_t
    if (i == atp_matches_2000_test$player_j_id[t-1]){
      lambda_b_pred[,i,t] = lambda_b_pred[,i,t-1] + tau_b*s_j
      lambda_c_pred[,i,t] = lambda_c_pred[,i,t-1] + tau_c*s_j*atp_matches_2000_test$I_clay[t-1]
      lambda_g_pred[,i,t] = lambda_g_pred[,i,t-1] + tau_g*s_j*atp_matches_2000_test$I_grass[t-1]
      lambda_h_pred[,i,t] = lambda_h_pred[,i,t-1] + tau_h*s_j*atp_matches_2000_test$I_hard[t-1]
    }
    #if player didn't play at time t, then copy their strength from t-1 to t
    if (i != atp_matches_2000_test$player_i_id[t-1] && i != atp_matches_2000_test$player_j_id[t-1]){
      lambda_b_pred[,i,t] = lambda_b_pred[,i,t-1]
      lambda_c_pred[,i,t] = lambda_c_pred[,i,t-1]
      lambda_g_pred[,i,t] = lambda_g_pred[,i,t-1]
      lambda_h_pred[,i,t] = lambda_h_pred[,i,t-1]
    }
  }
  #Combining the baseline and surface-specific strengths depending on the surface of the match at time t
  lambda_pred_i_temp <- lambda_b_pred[,atp_matches_2000_test$player_i_id[t],t] + atp_matches_2000_test$I_clay[t]*lambda_c_pred[,atp_matches_2000_test$player_i_id[t],t]+ atp_matches_2000_test$I_grass[t]*lambda_g_pred[,atp_matches_2000_test$player_i_id[t],t]+ atp_matches_2000_test$I_hard[t]*lambda_h_pred[,atp_matches_2000_test$player_i_id[t],t]
  
  lambda_pred_j_temp <- lambda_b_pred[,atp_matches_2000_test$player_j_id[t],t] + atp_matches_2000_test$I_clay[t]*lambda_c_pred[,atp_matches_2000_test$player_j_id[t],t]+ atp_matches_2000_test$I_grass[t]*lambda_g_pred[,atp_matches_2000_test$player_j_id[t],t]+ atp_matches_2000_test$I_hard[t]*lambda_h_pred[,atp_matches_2000_test$player_j_id[t],t]
  
  #Using the above strengths to calculate the predicted probability distribution for each match in the test set
  p_post_preds_exp_win[,t] <- exp(lambda_pred_i_temp+ beta_R*(atp_matches_2000_test$transformed_rank_i[t] - mean(atp_matches_2000_train$transformed_rank_i))/sqrt(var(atp_matches_2000_train$transformed_rank_i)+var(atp_matches_2000_train$transformed_rank_j)) + beta_B*(atp_matches_2000_test$bp_saved_ratio_i[t] - mean(atp_matches_2000_train$bp_saved_ratio_i))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_i)+var(atp_matches_2000_train$bp_saved_ratio_j)))/(exp(lambda_pred_i_temp + beta_R*(atp_matches_2000_test$transformed_rank_i[t] - mean(atp_matches_2000_train$transformed_rank_i))/sqrt(var(atp_matches_2000_train$transformed_rank_i)+var(atp_matches_2000_train$transformed_rank_j)) + beta_B*(atp_matches_2000_test$bp_saved_ratio_i[t] - mean(atp_matches_2000_train$bp_saved_ratio_i))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_i)+var(atp_matches_2000_train$bp_saved_ratio_j)))+exp(lambda_pred_j_temp+ beta_R*(atp_matches_2000_test$transformed_rank_j[t] - mean(atp_matches_2000_train$transformed_rank_j))/sqrt(var(atp_matches_2000_train$transformed_rank_j)+var(atp_matches_2000_train$transformed_rank_i)) + beta_B*(atp_matches_2000_test$bp_saved_ratio_j[t] - mean(atp_matches_2000_train$bp_saved_ratio_j))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_j)+var(atp_matches_2000_train$bp_saved_ratio_i))))
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

#plotting the ROC curve for the predictions based on the first classification approach
roc_curve <- roc(response = atp_matches_2000_test$y_pred_exp_win,predictor = as.numeric(atp_matches_2000_test$y)-1)
plot(roc_curve)

#plotting the ROC curve for the predictions based on the second classification approach
roc_curve <- roc(response = atp_matches_2000_test$y_pred_exp_win_2,predictor = as.numeric(atp_matches_2000_test$y)-1)
plot(roc_curve)



