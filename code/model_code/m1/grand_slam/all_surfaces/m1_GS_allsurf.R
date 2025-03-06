#========== This script file runs M1 on the combined surfaces data set for the Grand Slam Tour ==============

rm(list=ls()) #clearing environment
setwd("~/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/fixed_time_bt/grand_slam/grand_slam_allsurf")

library(jagsUI)
library(tidyverse)
library(tidybayes)

#importing the data
atp_matches_2000_BT <- read.csv2("atp_matches_2000_train_GS_allsurf.csv",header = TRUE)
atp_players_2000_BT <- read.csv2("atp_players_2000_train_GS_allsurf.csv",header = TRUE)

#taking the required subset of variables from the data
subset_data <- subset(atp_matches_2000_BT, select = c(winner_id,winner_name,loser_id,loser_name, player_i_name,player_i_id,player_j_id,player_j_name,y,Time))

#preparing the model data list 
model.data <- 
  subset_data %>%
  compose_data()

#taking length of all players being considered
n_players <- length(unique(c(atp_matches_2000_BT$player_i_id,atp_matches_2000_BT$player_j_id)))

model.data$n_players <- n_players #adding this to the list

#establishing the model
model <- "model{
  #likelihood
  for (t in 1:n){
    y[t] ~ dbern(p[t])
    
    logit(p[t]) <- lambda[player_i_id[t]] - lambda[player_j_id[t]]
  }
  
  #initial prior for all players and sum to 0 constraint
  for (i in 1:n_players){
      lambda.star[i] ~ dnorm(0,0.001)
      lambda[i] <- lambda.star[i] - mean(lambda.star[])
  }
}
"

#parameters to keep track
parms <- c("lambda","p")

#running MCMC
t1 <- Sys.time()
out <- jagsUI(data = model.data,
              parameters.to.save = parms,
              model.file = textConnection(model),
              n.chains = 3,
              n.adapt = 200,
              n.iter = 1800,
              n.burnin = 800,)

t2 <- Sys.time()
run_time <- t2-t1
save(out,model.data,run_time,file = "BT_fixed_time_out_GS_allsurf.RData")

#===========updating the model with more samples. RUN THIS SECTION TO UPDATE THE SAMPLES MORE============
#load(file = "/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_supercomp/Model_fitting_normal_priors/gorgi_noreg_nosurf/grand_slam/grand_slam_clay/gorgi_simple_model_output_GS_clay.RData")
#load(file = "/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/gorgi_noreg_nosurf/grand_slam/grand_slam_clay/gorgi_simple_model_output_GS_clay.RData")
#load(file = "/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/fixed_time_bt/grand_slam/grand_slam_allsurf/BT_fixed_time_out_GS_allsurf_with_extra_burnin.RData")

t11 <- Sys.time() #used to get the run time of the algorithm
#for (i in 1:8){
  out <- update(out,parameters.to.save = parms, n.iter = 25000,n.thin = 5) #updating the model
  save(out,model.data,file = "BT_fixed_time_out_GS_allsurf_with_extra_burnin_3.RData") #saving the outputs
#  print(i)
#}
t21 <- Sys.time()
run_time_2 <- t21-t11
save(out,model.data,run_time_2,file = "BT_fixed_time_out_GS_allsurf_with_extra_burnin_3.RData")
#===========================================================================================================