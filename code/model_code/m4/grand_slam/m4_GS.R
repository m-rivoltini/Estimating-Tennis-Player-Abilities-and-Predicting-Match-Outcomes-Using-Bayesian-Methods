#======= This script file implements M4 applied to the reduced Grand Slam data set combining all surfaces =======

#Clearing environment and setting working directory
rm(list=ls())
setwd("~/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/gorgi_reg_surf/grand_slam")

#loading packages
library(jagsUI)
library(tidyverse)
library(tidybayes)

#loading data
atp_matches_2000 <- read.csv2("atp_matches_2000_train_GS_allsurf_reg_surf_reduced.csv",header = TRUE)

#generating the data list for jagsUI
subset_data <- subset(atp_matches_2000, select = c(winner_id,winner_name,loser_id,loser_name, player_i_name,player_i_id,player_j_id,player_j_name,y,Time,transformed_rank_i,transformed_rank_j,diff_transformed_rank,age_i,age_j,diff_age,height_i,height_j,diff_height,bp_saved_ratio_i,bp_saved_ratio_j,diff_bp_saved_ratio,I_grass,I_clay,I_hard))

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
    
    logit(p_ij[t]) <- (lambda[player_i_id[t],t] - lambda[player_j_id[t],t]) + beta_R*(diff_transformed_rank[t]-mean(diff_transformed_rank[]))/sd(diff_transformed_rank[]) + beta_B*(diff_bp_saved_ratio[t]-mean(diff_bp_saved_ratio[]))/sd(diff_bp_saved_ratio[])
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
      
      lambda_b[i,t] <- (lambda_b[i, t-1] + tau_b * s_i[t-1])*cond1[i,t-1] + (lambda_b[i, t-1] + tau_b * s_j[t-1])*cond2[i,t-1] + lambda_b[i,t-1]*cond3[i,t-1]
      lambda_c[i,t] <- (lambda_c[i, t-1] + tau_c *I_clay[t-1]*s_i[t-1])*cond1[i,t-1] + (lambda_c[i, t-1] + tau_c *I_clay[t-1]*s_j[t-1])*cond2[i,t-1] + lambda_c[i,t-1]*cond3[i,t-1]
      lambda_h[i,t] <- (lambda_h[i, t-1] + tau_h *I_hard[t-1]*s_i[t-1])*cond1[i,t-1] + (lambda_h[i, t-1] + tau_h *I_hard[t-1]*s_j[t-1])*cond2[i,t-1] + lambda_h[i,t-1]*cond3[i,t-1]
      lambda_g[i,t] <- (lambda_g[i, t-1] + tau_g *I_grass[t-1]*s_i[t-1])*cond1[i,t-1] + (lambda_g[i, t-1] + tau_g *I_grass[t-1]*s_j[t-1])*cond2[i,t-1] + lambda_g[i,t-1]*cond3[i,t-1]
    
    }
  }
  
  for (i in 1:n_players){ 
    for (t in 2:n){
      lambda[i,t] = (lambda_b[player_i_id[t],t] + I_grass[t]*lambda_g[player_i_id[t],t] + I_clay[t]*lambda_c[player_i_id[t],t] + I_hard[t]*lambda_h[player_i_id[t],t])*cond1[i,t] + (lambda_b[player_j_id[t],t] + I_grass[t]*lambda_g[player_j_id[t],t] + I_clay[t]*lambda_c[player_j_id[t],t] + I_hard[t]*lambda_h[player_j_id[t],t])*cond2[i,t] + lambda[i,t-1]*cond3[i,t]
    }
  }
  
  #initial time point is set to baseline strengths
  for (i in 1:n_players){ 
    lambda[i,1] = (lambda_b[player_i_id[1],1] + I_grass[1]*lambda_g[player_i_id[1],1] + I_clay[1]*lambda_c[player_i_id[1],1] + I_hard[1]*lambda_h[player_i_id[1],1])*cond1[i,1] + (lambda_b[player_j_id[1],1] + I_grass[1]*lambda_g[player_j_id[1],1] + I_clay[1]*lambda_c[player_j_id[1],1] + I_hard[1]*lambda_h[player_j_id[1],1])*cond2[i,1] + lambda_b[i,1]*cond3[i,1]
  }
  
  for (i in 1:n_players){
    for (t in 2:n){
      lambda_tilde[i,t] = (lambda[i,t] + beta_R*(transformed_rank_i[t]-mean(transformed_rank_i[]))/sd(transformed_rank_i[]) + beta_B*(bp_saved_ratio_i[t]-mean(bp_saved_ratio_i[]))/sd(bp_saved_ratio_i[]))*cond1[i,t] + (lambda[i,t] + beta_R*(transformed_rank_j[t]-mean(transformed_rank_j[]))/sd(transformed_rank_j[]) + beta_B*(bp_saved_ratio_j[t]-mean(bp_saved_ratio_j[]))/sd(bp_saved_ratio_j[]))*cond2[i,t] + (lambda_tilde[i,t-1])*cond3[i,t]
    }
  }
  
  #setting lambda_tilde for the initial time point (only two players play at time 1. So we set lambda_tilde at time 1 for these players similar to the above, and the rest of the players we set them to lambda[i,t])
  
  for (i in 1:n_players){
    lambda_tilde[i,1] = (lambda[player_i_id[1],1] + beta_R*(transformed_rank_i[1]-mean(transformed_rank_i[]))/sd(transformed_rank_i[]) + beta_B*(bp_saved_ratio_i[1]-mean(bp_saved_ratio_i[]))/sd(bp_saved_ratio_i[]))*cond1[i,1] + (lambda[player_j_id[1],1] + beta_R*(transformed_rank_j[1]-mean(transformed_rank_j[]))/sd(transformed_rank_j[]) + beta_B*(bp_saved_ratio_j[1]-mean(bp_saved_ratio_j[]))/sd(bp_saved_ratio_j[]))*cond2[i,1] + (lambda[i,1])*cond3[i,1]
  }
  
  ####### PRIORS #######
  
  #initial prior for all players #add identifiability constraints 
  for (i in 1:n_players){
      lambda_b.star[i,1] ~ dnorm(0,0.001)
      lambda_b[i,1] <- lambda_b.star[i,1] - mean(lambda_b.star[,1])
     
      lambda_c.star[i,1] ~ dnorm(0,0.001)
      lambda_c[i,1] <- lambda_c.star[i,1] - mean(lambda_c.star[,1]) 
      
      lambda_g.star[i,1] ~ dnorm(0,0.001)
      lambda_g[i,1] <- lambda_g.star[i,1] - mean(lambda_g.star[,1])
      
      lambda_h.star[i,1] ~ dnorm(0,0.001)
      lambda_h[i,1] <- lambda_h.star[i,1] - mean(lambda_h.star[,1])
  }
  
  #prior for static variables
  tau_b ~ dnorm(0,0.05)T(0,)
  tau_c ~ dnorm(0,0.05)T(0,)
  tau_g ~ dnorm(0,0.05)T(0,)
  tau_h ~ dnorm(0,0.05)T(0,)
  beta_R ~ dnorm(0,0.0001)
  beta_B ~ dnorm(0,0.0001)
  #beta_a ~ dnorm(0,0.0001) - removed
  #beta_h ~ dnorm(0,0.0001) - removed
}
"

#parameters to keep track
parms <- c("lambda","p_ij","beta_R","beta_B","lambda_tilde","lambda_b","lambda_g","lambda_h","lambda_c","tau_b","tau_c","tau_g","tau_h")

#Running MCMC
t1 <- Sys.time() #used to get computation times
out <- jagsUI(data = model.data,
              parameters.to.save = parms,
              model.file = textConnection(model),
              n.chains = 3,
              n.adapt = 300,
              n.iter = 4500,
              n.burnin = 4000,)

t2 <- Sys.time()
run_time <- t2-t1

#saving outputs
save(out,model.data,run_time,file = "gorgi_model_reg_surf_GS_reduced_output_more_burnin.RData")


#===========updating the model with more samples. RUN THIS SECTION TO UPDATE THE SAMPLES MORE============
#The below is the original. Run the next line to use the previously obtained samples
#load(file = "/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_supercomp/Model_fitting_normal_priors/gorgi_noreg_nosurf/grand_slam/grand_slam_clay/gorgi_simple_model_output_GS_clay.RData")
#load(file = "/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/gorgi_reg_nosurf/grand_slam/grand_slam_clay/gorgi_model_reg_nosurf_output_GS_clay.RData")
#load("/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/gorgi_reg_surf/grand_slam/gorgi_model_reg_surf_GS_reduced_output.RData")

#This was not run for this model, since the 4000 burn-in iterations were performed straight away in the jagsUI command
t11 <- Sys.time()
run_time_3 <- mat.or.vec(8,1)
for (i in 1:8){
  t111 <- Sys.time()
  
  out <- update(out,parameters.to.save = parms, n.iter = 500)
  save(out,model.data,file = "gorgi_model_reg_surf_GS_reduced_output_extra_burnin.RData")
  
  t211 <- Sys.time()
  run_time_3[i] <- t211-t111
  
  print(i)
}
t21 <- Sys.time()
run_time_2 <- t21-t11
save(out,model.data,run_time_2,run_time_3,file = "gorgi_model_reg_surf_GS_reduced_output_extra_burnin.RData")
#===========================================================================================================
