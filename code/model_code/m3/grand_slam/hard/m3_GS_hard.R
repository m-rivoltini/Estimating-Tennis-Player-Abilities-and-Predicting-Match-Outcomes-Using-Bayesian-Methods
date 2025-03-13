#========== This script file runs M3 on the hard data set for the Grand Slam Tour ==========

#clearing environment and setting working directory
rm(list=ls())
setwd("~/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/gorgi_reg_nosurf/grand_slam/grand_slam_hard")

#loading packages
library(jagsUI)
library(tidyverse)
library(tidybayes)

#importing data
atp_matches_2000 <- read.csv2("atp_matches_2000_train_GS_hard.csv",header = TRUE)
atp_players_2000 <- read.csv2("atp_players_2000_train_GS_hard.csv",header = TRUE)

#generating data for jagsUI
subset_data <- subset(atp_matches_2000, select = c(winner_id,winner_name,loser_id,loser_name, player_i_name,player_i_id,player_j_id,player_j_name,y,Time,transformed_rank_i,transformed_rank_j,diff_transformed_rank,age_i,age_j,diff_age,height_i,height_j,diff_height,bp_saved_ratio_i,bp_saved_ratio_j,diff_bp_saved_ratio))#consider adding difference in break points won

#preparing the model data list 
model.data <- 
  subset_data %>%
  rename(y_ij=y) %>%
  compose_data()

#taking length of all players being considered
n_players <- length(unique(c(atp_matches_2000$player_i_id,atp_matches_2000$player_j_id)))

model.data$n_players <- n_players #adding this to the list

#constructing the model
model <- "model{
  
  ####### LIKELIHOOD ########
  for (t in 1:n){
    y_ij[t] ~ dbern(p_ij[t])
    
    logit(p_ij[t]) <- (lambda[player_i_id[t],t] - lambda[player_j_id[t],t]) + beta_R*(diff_transformed_rank[t] - mean(diff_transformed_rank[]))/sd(diff_transformed_rank[]) + beta_B*(diff_bp_saved_ratio[t] - mean(diff_bp_saved_ratio[]))/sd(diff_bp_saved_ratio[])
  }
  
  
  
  #####updating of strengths####
  
  for (t in 1:n){
    s_i[t] <- y_ij[t]*(1-p_ij[t]) - (1 - y_ij[t])*p_ij[t]
    s_j[t] <- -s_i[t]
  }
  
  #these shall be used to assign lambda[i,t] depending on whether i is player i_t or j_t at time t
  for (i in 1:n_players){
    for (t in 1:n){
      cond1[i,t] <- ifelse(i==player_i_id[t],1,0) #equal to 1 if i=i_t 
      cond2[i,t] <- ifelse(i==player_j_id[t],1,0) #equal to 1 if i=j_t
      cond3[i,t] <- ifelse(i!=player_i_id[t] && i!=player_j_id[t],1,0) #equal to 1 if i!=i_t and i!=j_t (i did not play at time t)
    }
  }
  
  for (i in 1:n_players){
    for (t in 2:n){
      lambda[i,t] <- (lambda[i,t-1] + tau * s_i[t-1])*cond1[i,t-1] + (lambda[i, t-1] + tau * s_j[t-1])*cond2[i,t-1] + lambda[i,t-1]*cond3[i,t-1]
    }
  }
  
  #The computation of lambda_tilde requires further processing steps which are performed in the analysing_out script files
  for (i in 1:n_players){
    for (t in 2:n){
      lambda_tilde[i,t] = (lambda[i,t] + beta_R*(transformed_rank_i[t] - mean(transformed_rank_i[]))/sd(transformed_rank_i[]) + beta_B*(bp_saved_ratio_i[t] - mean(bp_saved_ratio_i[]))/sd(bp_saved_ratio_i[]))*cond1[i,t] + (lambda[i,t] + beta_R*(transformed_rank_j[t] - mean(transformed_rank_j[]))/sd(transformed_rank_j[]) + beta_B*(bp_saved_ratio_j[t] - mean(bp_saved_ratio_j[]))/sd(bp_saved_ratio_j[]))*cond2[i,t] + (lambda_tilde[i,t-1])*cond3[i,t] 
    }
  }
  
  #setting lambda_tilde for the initial time point (only two players play at time 1. So we set lambda_tilde at time 1 for these players similar to the above, and the rest of the players we set them to lambda[i,t])
  
  for (i in 1:n_players){
    lambda_tilde[i,1] = (lambda[player_i_id[1],1] + beta_R*(transformed_rank_i[1] - mean(transformed_rank_i[]))/sd(transformed_rank_i[]) + beta_B*(bp_saved_ratio_i[1] - mean(bp_saved_ratio_i[]))/sd(bp_saved_ratio_i[]))*cond1[i,1] + (lambda[player_j_id[1],1] + beta_R*(transformed_rank_j[1] - mean(transformed_rank_j[]))/sd(transformed_rank_j[]) + beta_B*(bp_saved_ratio_j[1] - mean(bp_saved_ratio_j[]))/sd(bp_saved_ratio_j[]))*cond2[i,1] + (lambda[i,1])*cond3[i,1]
  }
  
  ####### PRIORS #######
  
  #initial prior for all players and sum to 0 constraint
  for (i in 1:n_players){
      lambda.star[i,1] ~ dnorm(0,0.001)
      lambda[i,1] <- lambda.star[i,1] - mean(lambda.star[,1])
  }
  
  #prior for static variables
  tau ~ dnorm(0,0.05)T(0,)
  beta_R ~ dnorm(0,0.0001)
  beta_B ~ dnorm(0,0.0001)
  #beta_a ~ dnorm(0,0.0001) - removed
  #beta_h ~ dnorm(0,0.0001) - removed
}
"

#parameters to keep track
parms <- c("lambda","p_ij","beta_R","beta_B","lambda_tilde","tau")

#Running MCMC
t1 <- Sys.time()
out <- jagsUI(data = model.data,
              parameters.to.save = parms,
              model.file = textConnection(model),
              n.chains = 3,
              n.adapt = 300,
              n.iter = 1200,
              n.burnin = 1000)

t2 <- Sys.time()
run_time <- t2-t1

#saving outputs
save(out,model.data,run_time,file = "gorgi_model_reg_nosurf_output_GS_hard_reduced_stand_pred_noht_noage_out.RData")


#===========updating the model with more samples. RUN THIS SECTION TO UPDATE THE SAMPLES MORE============
#The below is the original. Run the next line to use the previously obtained samples
#load(file = "/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_supercomp/Model_fitting_normal_priors/gorgi_noreg_nosurf/grand_slam/grand_slam_clay/gorgi_simple_model_output_GS_clay.RData")
#load(file = "/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/gorgi_reg_nosurf/grand_slam/grand_slam_clay/gorgi_model_reg_nosurf_output_GS_clay.RData")
#load("/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/gorgi_reg_nosurf/grand_slam/grand_slam_hard/gorgi_reg_nosurf_GS_hard_reduced_with_extra_burnin_stand_pred_noht_noage_2.RData")

t11 <- Sys.time() #used to obtain computation time
for (i in 1:8){
  out <- update(out,parameters.to.save = parms, n.iter = 1000)
  save(out,model.data,file = "gorgi_reg_nosurf_GS_hard_reduced_with_extra_burnin_stand_pred_noht_noage_4.RData")
  print(i)
}
t21 <- Sys.time()
run_time_2 <- t21-t11
save(out,model.data,run_time_2,file = "gorgi_reg_nosurf_GS_hard_reduced_with_extra_burnin_stand_pred_noht_noage_4.RData")
#===========================================================================================================
