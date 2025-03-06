#========== This script file runs M2 on the clay data set for the Masters Tour ==========

#Clearing environment and setting working directory
rm(list = ls())
setwd("~/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/gorgi_noreg_nosurf/master/master_clay")

#loading packages
library(jagsUI)
library(tidyverse)
library(tidybayes)

#importing the data
atp_matches_2000 <- read.csv2("atp_matches_2000_train_MS_clay.csv",header = TRUE)
atp_players_2000 <- read.csv2("atp_players_2000_train_MS_clay.csv",header = TRUE)

#generating the data for jagsUI
subset_data <- subset(atp_matches_2000, select = c(winner_id,winner_name,loser_id,loser_name, player_i_name,player_i_id,player_j_id,player_j_name,y,Time))

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
    
    logit(p_ij[t]) <- lambda[player_i_id[t],t] - lambda[player_j_id[t],t]
  }
  
  
  
  #####updating of strengths####
  
  for (t in 1:n){
    s_i[t] <- y_ij[t]*(1-p_ij[t]) - (1 - y_ij[t])*p_ij[t]
    s_j[t] <- -s_i[t]
  }
  for (i in 1:n_players){
    for (t in 1:n){
      cond1[i,t] <- ifelse(i==player_i_id[t],1,0) #these shall be used to assign lambda[i,t] depending on whether 
      cond2[i,t] <- ifelse(i==player_j_id[t],1,0)
      cond3[i,t] <- ifelse(i!=player_i_id[t] && i!=player_j_id[t],1,0)
    }
  }
  for (i in 1:n_players){
    for (t in 2:n){
    
      lambda[i,t] <- (lambda[i,t-1] + tau * s_i[t-1])*cond1[i,t-1] + (lambda[i,t-1] + tau * s_j[t-1])*cond2[i,t-1] + lambda[i,t-1]*cond3[i,t-1] 
    }
  }
  ####### PRIORS #######
  
  #initial prior for all players and sum to 0 constraint
  for (i in 1:n_players){
      lambda.star[i,1] ~ dnorm(0,0.001)
      lambda[i,1] <- lambda.star[i,1] - mean(lambda.star[,1])
  }
  
  #prior for tau
  tau ~ dnorm(0,0.05)T(0,) 
}
"

#parameters to keep track
parms <- c("lambda","p_ij","tau")

#Running MCMC
t1 <- Sys.time() #used to obtain computation time
out <- jagsUI(data = model.data,
              parameters.to.save = parms,
              model.file = textConnection(model),
              n.chains = 3,
              n.adapt = 200,
              n.iter = 1800,
              n.burnin = 800)

t2 <- Sys.time()
run_time <- t2-t1

#saving output
save(out,model.data,run_time,file = "gorgi_simple_model_output_MS_clay.RData")

#===========updating the model with more samples. RUN THIS SECTION TO UPDATE THE SAMPLES MORE============
#load(file = "/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_supercomp/Model_fitting_normal_priors/gorgi_noreg_nosurf/grand_slam/grand_slam_clay/gorgi_simple_model_output_GS_clay.RData")
#load(file = "/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/gorgi_noreg_nosurf/grand_slam/grand_slam_clay/gorgi_simple_model_output_GS_clay.RData")

t11 <- Sys.time() #used to obtain computation time
for (i in 1:8){
  out <- update(out,parameters.to.save = parms, n.iter = 1000)
  save(out,model.data,file = "gorgi_no_reg_nosurf_MS_clay_with_extra_burnin.RData")
  print(i)
}
t21 <- Sys.time()
run_time_2 <- t21-t11
save(out,model.data,run_time_2,file = "gorgi_no_reg_nosurf_MS_clay_with_extra_burnin.RData")
#===========================================================================================================
