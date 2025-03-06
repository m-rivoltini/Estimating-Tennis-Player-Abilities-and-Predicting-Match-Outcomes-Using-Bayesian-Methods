# ================= Analysing outputs of M4 applied to the reduced Grand Slam data set (2020-2022) ===========

#clearing environment and loading packages
rm(list=ls())
library(jagshelper)
library(jagsUI)
library(coda)
library(HDInterval)
library(caret)
library(dplyr)

#setting working directory
setwd("~/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Analysing_model_fit_outputs/gorgi_reg_surf/grand_slam")

#loading outputs and obtained run times for each compilation
load("/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/gorgi_reg_surf/grand_slam/gorgi_model_reg_surf_GS_reduced_output_engineering_extra_burn_in.RData")
run_time

#importing data 
atp_matches_2000_train <- read.csv2("/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/gorgi_reg_surf/grand_slam/atp_matches_2000_train_GS_allsurf_reg_surf_reduced.csv")

atp_players_2000_train <- read.csv2("/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/gorgi_reg_surf/grand_slam/atp_players_2000_train_GS_allsurf_reg_surf_reduced.csv")

#ordering the player IDs
atp_players_2000_train <- atp_players_2000_train[order(atp_players_2000_train$player_id),]

#======================================================================================================#
#==============================================Analysis================================================#
#======================================================================================================#

#summary statistics of output - note that lambda_tilde obtained from here is incorrect, so we adjust it
summary(out) 

#checking the maximum rhat values for the parameters
max(out$Rhat$lambda_b)
length(which(out$Rhat$lambda_b>1.1))
max(out$Rhat$lambda_c)
length(which(out$Rhat$lambda_c>1.1))
max(out$Rhat$lambda_g)
length(which(out$Rhat$lambda_g>1.1))
max(out$Rhat$lambda_h)
length(which(out$Rhat$lambda_h>1.1))

#finding total number of parameters with rhat>1.1
length(which(out$Rhat$lambda_b>1.1))+length(which(out$Rhat$lambda_c>1.1))+length(which(out$Rhat$lambda_g>1.1))+length(which(out$Rhat$lambda_h>1.1))

max(out$Rhat$lambda)
length(which(out$Rhat$lambda>1.1))

max(out$Rhat$lambda_tilde)

max(out$Rhat$p_ij)
length(which(out$Rhat$p_ij>1.1))

max(out$Rhat$tau_b)
max(out$Rhat$tau_c)
max(out$Rhat$tau_h)
max(out$Rhat$tau_g)

max(out$Rhat$beta_R)
max(out$Rhat$beta_B)

#======== Fixing lambda_tilde ======

lambda <- out$sims.list$lambda
beta_R <- out$sims.list$beta_R
beta_B <- out$sims.list$beta_B

lambda_tilde_sims <- array(NA, dim = dim(lambda))

#calculating lambda_tilde for the initial time point using the correct scaling 
for (i in 1:nrow(atp_players_2000_train)){
  
  if (i == atp_matches_2000_train$player_i_id[1]){
    lambda_tilde_sims[,i,1] = lambda[,i,1] + beta_R*(atp_matches_2000_train$transformed_rank_i[1]-mean(atp_matches_2000_train$transformed_rank_i))/sqrt(var(atp_matches_2000_train$transformed_rank_i)+var(atp_matches_2000_train$transformed_rank_j)) + beta_B*(atp_matches_2000_train$bp_saved_ratio_i[1]-mean(atp_matches_2000_train$bp_saved_ratio_i))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_i)+var(atp_matches_2000_train$bp_saved_ratio_j))
  }
  
  if (i == atp_matches_2000_train$player_j_id[1]){
    lambda_tilde_sims[,i,1] = lambda[,i,1] + beta_R*(atp_matches_2000_train$transformed_rank_j[1]-mean(atp_matches_2000_train$transformed_rank_j))/sqrt(var(atp_matches_2000_train$transformed_rank_i)+var(atp_matches_2000_train$transformed_rank_j)) + beta_B*(atp_matches_2000_train$bp_saved_ratio_j[1]-mean(atp_matches_2000_train$bp_saved_ratio_j))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_i)+var(atp_matches_2000_train$bp_saved_ratio_j))
  }
  
  if (i != atp_matches_2000_train$player_i_id[1] & i != atp_matches_2000_train$player_j_id[1]){
    lambda_tilde_sims[,i,1] = lambda[,i,1]
  }
  
}

#repeating for all time points
for (i in 1:nrow(atp_players_2000_train)){
  for (t in 2:nrow(atp_matches_2000_train)){
    
    if (i == atp_matches_2000_train$player_i_id[t]){
      lambda_tilde_sims[,i,t] = lambda[,i,t] + beta_R*(atp_matches_2000_train$transformed_rank_i[t]-mean(atp_matches_2000_train$transformed_rank_i))/sqrt(var(atp_matches_2000_train$transformed_rank_i)+var(atp_matches_2000_train$transformed_rank_j)) + beta_B*(atp_matches_2000_train$bp_saved_ratio_i[t]-mean(atp_matches_2000_train$bp_saved_ratio_i))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_i)+var(atp_matches_2000_train$bp_saved_ratio_j))
    }
    
    if (i == atp_matches_2000_train$player_j_id[t]){
      lambda_tilde_sims[,i,t] = lambda[,i,t] + beta_R*(atp_matches_2000_train$transformed_rank_j[t]-mean(atp_matches_2000_train$transformed_rank_j))/sqrt(var(atp_matches_2000_train$transformed_rank_i)+var(atp_matches_2000_train$transformed_rank_j)) + beta_B*(atp_matches_2000_train$bp_saved_ratio_j[t]-mean(atp_matches_2000_train$bp_saved_ratio_j))/sqrt(var(atp_matches_2000_train$bp_saved_ratio_i)+var(atp_matches_2000_train$bp_saved_ratio_j))
    }
    
    if (i != atp_matches_2000_train$player_i_id[t] & i != atp_matches_2000_train$player_j_id[t]){
      lambda_tilde_sims[,i,t] = lambda_tilde_sims[,i,t-1]
    }
    
  }
}

#converting the 3D array into a 2D matrix and converting to data frame
lambda_tilde_sims_flat <- matrix(lambda_tilde_sims,nrow = dim(lambda_tilde_sims)[1],ncol = dim(lambda_tilde_sims)[2]*dim(lambda_tilde_sims)[3])
lambda_tilde_sims_flat <- as.data.frame(lambda_tilde_sims_flat)

#changing the column names to match the parameters
colnames(lambda_tilde_sims_flat) <- with(expand.grid(player = 1:nrow(atp_players_2000_train), time = 1:nrow(atp_matches_2000_train)), paste0("lambda_tilde[",player,",",time,"]"))

#creating the MCMC chains 
chain_1 <- mcmc(lambda_tilde_sims_flat[1:500,,])
chain_2 <- mcmc(lambda_tilde_sims_flat[501:1000,,])
chain_3 <- mcmc(lambda_tilde_sims_flat[1001:1500,,])

#creating the mcmc.list object
lambda_tilde_mcmc <- mcmc.list(chain_1,chain_2,chain_3)

#getting the posterior means for each time point and player
lambda_tilde <- colMeans(lambda_tilde_sims,dim=1)


#calculating the summary statistics for lambda_tilde
lambda_tilde_ess <- effectiveSize(lambda_tilde_mcmc)
lambda_tilde_rhat <- sapply(colnames(lambda_tilde_sims_flat),function(param){gelman.diag(lambda_tilde_mcmc[,param])$psrf[1]})
lambda_tilde_means <- colMeans(lambda_tilde_sims_flat)
lambda_tilde_sd <- sapply(lambda_tilde_sims_flat,sd)
lambda_tilde_2.5 <- sapply(lambda_tilde_sims_flat,function(x) quantile(x, probs = 0.025))
lambda_tilde_97.5 <- sapply(lambda_tilde_sims_flat,function(x) quantile(x, probs = 0.975))

#creating the summary table for lambda_tilde
lambda_tilde_summary <- cbind(lambda_tilde_means,lambda_tilde_sd,lambda_tilde_2.5,lambda_tilde_97.5,lambda_tilde_rhat,lambda_tilde_ess)




#saving the summaries
summaries <- as.data.frame(out$summary) 

#obtaining a list of player names and matchups at each time point
names_times <- expand.grid(name = atp_players_2000_train$name[order(atp_players_2000_train$last_name)], time_point = 1:nrow(atp_matches_2000_train))
names_times <- paste(names_times[,1], names_times[,2]) 

#Finding the player names corresponding to the strengths and the match ups (i_t vs j_t) for p_i_tj_t
player_names_opponents <- c(names_times,paste(atp_matches_2000_train$player_i_name,"vs",atp_matches_2000_train$player_j_name),"beta_R","beta_B",paste("~",names_times),paste("baseline",names_times),paste("grass",names_times),paste("hard",names_times),paste("clay",names_times),"tau_b","tau_c","tau_g","tau_h","NA") #NA here is for the deviance

summaries$names <- player_names_opponents #appending names and match ups
summaries <- subset(summaries,select = c("names","mean","sd","2.5%","97.5%","Rhat","n.eff")) #reordering columns

#replace incorrect summaries corresponding to lambda_tilde
start_row <- which(rownames(summaries) == "lambda_tilde[1,1]")
end_row <- which(rownames(summaries) == paste0("lambda_tilde[",nrow(atp_players_2000_train),",",nrow(atp_matches_2000_train),"]"))
summaries[start_row:end_row,2:7] <- lambda_tilde_summary

#saving summaries
write.csv(summaries,file = "M4_GS_summaries.csv")

#exporting mean abilities and finding maximum and average abilities
lambda <- as.data.frame(out$mean$lambda) #means of lambda 
lambda$max_strength <- apply(lambda,1,max)
lambda$av_strength <- apply(lambda,1,mean)

lambda_b <- as.data.frame(out$mean$lambda_b) #means of lambda_b
lambda_b$max_strength <- apply(lambda_b,1,max)
lambda_b$av_strength <- apply(lambda_b,1,mean)

lambda_c <- as.data.frame(out$mean$lambda_c) #means of lambda_c
lambda_c$max_strength <- apply(lambda_c,1,max)
lambda_c$av_strength <- apply(lambda_c,1,mean)

lambda_g <- as.data.frame(out$mean$lambda_g) #means of lambda_g
lambda_g$max_strength <- apply(lambda_g,1,max)
lambda_g$av_strength <- apply(lambda_g,1,mean)

lambda_h <- as.data.frame(out$mean$lambda_h) #means of lambda_h
lambda_h$max_strength <- apply(lambda_h,1,max)
lambda_h$av_strength <- apply(lambda_h,1,mean)


lambda_tilde <- as.data.frame(lambda_tilde) #means of lambda_tilde
lambda_tilde$max_strength <- apply(lambda_tilde,1,max)
lambda_tilde$av_strength <- apply(lambda_tilde,1,mean)


p <- as.data.frame(out$mean$p) #means of p_ij's

#===================showing some plots and convergence diagnostics for the top players============================#

#ordering player IDs based on the average posterior means across the entire period
top_players_ID <- order(lambda_tilde$av_strength,decreasing = TRUE)#[1:10]

#getting the winner names from the ids
top_player_names <- atp_players_2000_train$name[match(top_players_ID, atp_players_2000_train$player_id)] 

#Plotting evolution of player baseline abilities for top 5 players
for (k in 1:5){
  plot(1:(ncol(lambda_b)-2),lambda_b[top_players_ID[k],1:(ncol(lambda_b)-2)],type="l",xlab = "Time",ylab = "Baseline Ability",main = paste("Evolution of Ability for",top_player_names[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda_b[top_players_ID[k],1:(ncol(lambda_b)-2)])) != 0)
  change_times <- (1:(ncol(lambda_b)-2))[change_points]
  points(change_times,lambda_b[top_players_ID[k],change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}

#Plotting evolution of player clay abilities for top 5 players
for (k in 1:5){
  plot(1:(ncol(lambda_c)-2),lambda_c[top_players_ID[k],1:(ncol(lambda_c)-2)],type="l",xlab = "Time",ylab = "Clay Ability",main = paste("Evolution of Ability for",top_player_names[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda_c[top_players_ID[k],1:(ncol(lambda_c)-2)])) != 0)
  change_times <- (1:(ncol(lambda_c)-2))[change_points]
  points(change_times,lambda_c[top_players_ID[k],change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}

#Plotting evolution of player grass abilities for top 5 players
for (k in 1:5){
  plot(1:(ncol(lambda_g)-2),lambda_g[top_players_ID[k],1:(ncol(lambda_g)-2)],type="l",xlab = "Time",ylab = "Grass Ability",main = paste("Evolution of Ability for",top_player_names[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda_g[top_players_ID[k],1:(ncol(lambda_g)-2)])) != 0)
  change_times <- (1:(ncol(lambda_g)-2))[change_points]
  points(change_times,lambda_g[top_players_ID[k],change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}

#Plotting evolution of player hard abilities for top 5 players
for (k in 1:5){
  plot(1:(ncol(lambda_h)-2),lambda_h[top_players_ID[k],1:(ncol(lambda_h)-2)],type="l",xlab = "Time",ylab = "Hard Ability",main = paste("Evolution of Ability for",top_player_names[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda_h[top_players_ID[k],1:(ncol(lambda_h)-2)])) != 0)
  change_times <- (1:(ncol(lambda_h)-2))[change_points]
  points(change_times,lambda_h[top_players_ID[k],change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}

#Plotting evolution of player intercept abilities for top 5 players
for (k in 1:5){
  plot(1:(ncol(lambda)-2),lambda_h[top_players_ID[k],1:(ncol(lambda)-2)],type="l",xlab = "Time",ylab = "Ability",main = paste("Evolution of Ability for",top_player_names[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda[top_players_ID[k],1:(ncol(lambda)-2)])) != 0)
  change_times <- (1:(ncol(lambda)-2))[change_points]
  points(change_times,lambda[top_players_ID[k],change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}

#Plotting evolution of player abilities for top 5 players
for (k in 1:5){
  plot(1:(ncol(lambda_tilde)-2),lambda_tilde[top_players_ID[k],1:(ncol(lambda_tilde)-2)],type="l",xlab = "Time",ylab = "Complete Ability",main = paste("Evolution of Ability for",top_player_names[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda_tilde[top_players_ID[k],1:(ncol(lambda_tilde)-2)])) != 0)
  change_times <- (1:(ncol(lambda_tilde)-2))[change_points]
  points(change_times,lambda_tilde[top_players_ID[k],change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}


pdf("M4_GS_seq_plots_lambda_b_top_players_ordered.pdf")
par(mfrow = c(3,1))
for (k in 1:nrow(atp_players_2000_train)){
  plot(1:(ncol(lambda_b)-2),lambda_b[top_players_ID[k],1:(ncol(lambda_b)-2)],type="l",xlab = "Time",ylab = "Baseline Ability",main = paste("Evolution of Ability for",top_player_names[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda_b[top_players_ID[k],1:(ncol(lambda_b)-2)])) != 0)
  change_times <- (1:(ncol(lambda_b)-2))[change_points]
  points(change_times,lambda_b[top_players_ID[k],change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}
dev.off()

pdf("M4_GS_clay_lambda_b_seq_plots.pdf")
par(mfrow = c(3,1))
for (k in 1:nrow(atp_players_2000_train)){
  plot(1:(ncol(lambda_b)-2),lambda_b[k,1:(ncol(lambda_b)-2)],type="l",xlab = "Time",ylab = "Baseline Ability",main = paste("Evolution of Ability for",atp_players_2000_train$name[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda_b[k,1:(ncol(lambda_b)-2)])) != 0)
  change_times <- (1:(ncol(lambda_b)-2))[change_points]
  points(change_times,lambda_b[k,change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}
dev.off()

pdf("M4_GS_clay_seq_plots_lambda_c_top_players_ordered.pdf")
par(mfrow = c(3,1))
for (k in 1:nrow(atp_players_2000_train)){
  plot(1:(ncol(lambda_c)-2),lambda_c[top_players_ID[k],1:(ncol(lambda_c)-2)],type="l",xlab = "Time",ylab = "Clay Ability",main = paste("Evolution of Ability for",top_player_names[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda_c[top_players_ID[k],1:(ncol(lambda_c)-2)])) != 0)
  change_times <- (1:(ncol(lambda_c)-2))[change_points]
  points(change_times,lambda_c[top_players_ID[k],change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}
dev.off()

pdf("M4_GS_clay_lambda_c_seq_plots.pdf")
par(mfrow = c(3,1))
for (k in 1:nrow(atp_players_2000_train)){
  plot(1:(ncol(lambda_c)-2),lambda_c[k,1:(ncol(lambda_c)-2)],type="l",xlab = "Time",ylab = "Clay Ability",main = paste("Evolution of Ability for",atp_players_2000_train$name[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda_c[k,1:(ncol(lambda_c)-2)])) != 0)
  change_times <- (1:(ncol(lambda_c)-2))[change_points]
  points(change_times,lambda_c[k,change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}
dev.off()

pdf("M4_GS_clay_seq_plots_lambda_g_top_players_ordered.pdf")
par(mfrow = c(3,1))
for (k in 1:nrow(atp_players_2000_train)){
  plot(1:(ncol(lambda_g)-2),lambda_g[top_players_ID[k],1:(ncol(lambda_g)-2)],type="l",xlab = "Time",ylab = "Grass Ability",main = paste("Evolution of Ability for",top_player_names[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda_g[top_players_ID[k],1:(ncol(lambda_g)-2)])) != 0)
  change_times <- (1:(ncol(lambda_g)-2))[change_points]
  points(change_times,lambda_g[top_players_ID[k],change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}
dev.off()

pdf("M4_GS_clay_lambda_g_seq_plots.pdf")
par(mfrow = c(3,1))
for (k in 1:nrow(atp_players_2000_train)){
  plot(1:(ncol(lambda_g)-2),lambda_g[k,1:(ncol(lambda_g)-2)],type="l",xlab = "Time",ylab = "Grass Ability",main = paste("Evolution of Ability for",atp_players_2000_train$name[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda_g[k,1:(ncol(lambda_g)-2)])) != 0)
  change_times <- (1:(ncol(lambda_g)-2))[change_points]
  points(change_times,lambda_g[k,change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}
dev.off()

pdf("M4_GS_clay_seq_plots_lambda_h_top_players_ordered.pdf")
par(mfrow = c(3,1))
for (k in 1:nrow(atp_players_2000_train)){
  plot(1:(ncol(lambda_h)-2),lambda_h[top_players_ID[k],1:(ncol(lambda_h)-2)],type="l",xlab = "Time",ylab = "Hard Ability",main = paste("Evolution of Ability for",top_player_names[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda_h[top_players_ID[k],1:(ncol(lambda_h)-2)])) != 0)
  change_times <- (1:(ncol(lambda_h)-2))[change_points]
  points(change_times,lambda_h[top_players_ID[k],change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}
dev.off()

pdf("M4_GS_clay_lambda_h_seq_plots.pdf")
par(mfrow = c(3,1))
for (k in 1:nrow(atp_players_2000_train)){
  plot(1:(ncol(lambda_h)-2),lambda_h[k,1:(ncol(lambda_h)-2)],type="l",xlab = "Time",ylab = "Hard Ability",main = paste("Evolution of Ability for",atp_players_2000_train$name[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda_h[k,1:(ncol(lambda_h)-2)])) != 0)
  change_times <- (1:(ncol(lambda_h)-2))[change_points]
  points(change_times,lambda_h[k,change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}
dev.off()

pdf("M4_GS_clay_seq_plots_lambda_top_players_ordered.pdf")
par(mfrow = c(3,1))
for (k in 1:nrow(atp_players_2000_train)){
  plot(1:(ncol(lambda)-2),lambda[top_players_ID[k],1:(ncol(lambda)-2)],type="l",xlab = "Time",ylab = "Ability",main = paste("Evolution of Ability for",top_player_names[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda[top_players_ID[k],1:(ncol(lambda)-2)])) != 0)
  change_times <- (1:(ncol(lambda)-2))[change_points]
  points(change_times,lambda[top_players_ID[k],change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}
dev.off()

pdf("M4_GS_clay_seq_plots_lambda.pdf")
par(mfrow = c(3,1))
for (k in 1:nrow(atp_players_2000_train)){
  plot(1:(ncol(lambda)-2),lambda[k,1:(ncol(lambda)-2)],type="l",xlab = "Time",ylab = "Ability",main = paste("Evolution of Ability for",atp_players_2000_train$name[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda[k,1:(ncol(lambda)-2)])) != 0)
  change_times <- (1:(ncol(lambda)-2))[change_points]
  points(change_times,lambda[k,change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}
dev.off()

pdf("M4_GS_clay_seq_plots_lambda_tilde_top_players_ordered.pdf")
par(mfrow = c(3,1))
for (k in 1:nrow(atp_players_2000_train)){
  plot(1:(ncol(lambda_tilde)-2),lambda_tilde[top_players_ID[k],1:(ncol(lambda_tilde)-2)],type="l",xlab = "Time",ylab = "Complete Ability",main = paste("Evolution of Ability for",top_player_names[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda_tilde[top_players_ID[k],1:(ncol(lambda_tilde)-2)])) != 0)
  change_times <- (1:(ncol(lambda_tilde)-2))[change_points]
  points(change_times,lambda_tilde[top_players_ID[k],change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}
dev.off()

pdf("M4_GS_clay_lambda_tilde_seq_plots.pdf")
par(mfrow = c(3,1))
for (k in 1:nrow(atp_players_2000_train)){
  plot(1:(ncol(lambda_tilde)-2),lambda_tilde[k,1:(ncol(lambda_tilde)-2)],type="l",xlab = "Time",ylab = "Complete Ability",main = paste("Evolution of Ability for",atp_players_2000_train$name[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda_tilde[k,1:(ncol(lambda_tilde)-2)])) != 0)
  change_times <- (1:(ncol(lambda_tilde)-2))[change_points]
  points(change_times,lambda_tilde[k,change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}
dev.off()

pdf("M4_GS_clay_lambda_b_density_plots.pdf")
par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    coda::densplot(out$samples[,paste0("lambda_b[",i,",",t,"]")])
    mtext(paste(paste0("lambda_b[",i,",",t,"]")," ",as.character(atp_players_2000_train$name[i]),"at time",t),side = 3,cex = 0.6)
  }
}
dev.off()

pdf("M4_GS_clay_lambda_c_density_plots.pdf")
par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    coda::densplot(out$samples[,paste0("lambda_c[",i,",",t,"]")])
    mtext(paste(paste0("lambda_c[",i,",",t,"]")," ",as.character(atp_players_2000_train$name[i]),"at time",t),side = 3,cex = 0.6)
  }
}
dev.off()

pdf("M4_GS_clay_lambda_g_density_plots.pdf")
par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    coda::densplot(out$samples[,paste0("lambda_g[",i,",",t,"]")])
    mtext(paste(paste0("lambda_g[",i,",",t,"]")," ",as.character(atp_players_2000_train$name[i]),"at time",t),side = 3,cex = 0.6)
  }
}
dev.off()

pdf("M4_GS_clay_lambda_h_density_plots.pdf")
par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    coda::densplot(out$samples[,paste0("lambda_h[",i,",",t,"]")])
    mtext(paste(paste0("lambda_h[",i,",",t,"]")," ",as.character(atp_players_2000_train$name[i]),"at time",t),side = 3,cex = 0.6)
  }
}
dev.off()

pdf("M4_GS_clay_lambda_density_plots.pdf")
par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    coda::densplot(out$samples[,paste0("lambda[",i,",",t,"]")])
    mtext(paste(paste0("lambda[",i,",",t,"]")," ",as.character(atp_players_2000_train$name[i]),"at time",t),side = 3,cex = 0.6)
  }
}
dev.off()

pdf("M4_GS_clay_lambda_tilde_density_plots.pdf")
par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    coda::densplot(lambda_tilde_mcmc[,paste0("lambda_tilde[",i,",",t,"]")])
    mtext(paste(paste0("lambda_tilde[",i,",",t,"]")," ",as.character(atp_players_2000_train$name[i]),"at time",t),side = 3,cex = 0.6)
  }
}
dev.off()


pdf("M4_GS_clay_lambda_b_trace_plots.pdf")
par(mfrow = c(3,1))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    coda::traceplot(out$samples[,paste0("lambda_b[",i,",",t,"]")])
    mtext(paste(paste0("lambda_b[",i,",",t,"]"),as.character(atp_players_2000_train$name[i]),t),side = 3,cex = 1.2)
  }
}
dev.off()

pdf("M4_GS_clay_lambda_c_trace_plots.pdf")
par(mfrow = c(3,1))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    coda::traceplot(out$samples[,paste0("lambda_c[",i,",",t,"]")])
    mtext(paste(paste0("lambda_c[",i,",",t,"]"),as.character(atp_players_2000_train$name[i]),t),side = 3,cex = 1.2)
  }
}
dev.off()

pdf("M4_GS_clay_lambda_g_trace_plots.pdf")
par(mfrow = c(3,1))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    coda::traceplot(out$samples[,paste0("lambda_g[",i,",",t,"]")])
    mtext(paste(paste0("lambda_g[",i,",",t,"]"),as.character(atp_players_2000_train$name[i]),t),side = 3,cex = 1.2)
  }
}
dev.off()

pdf("M4_GS_clay_lambda_h_trace_plots.pdf")
par(mfrow = c(3,1))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    coda::traceplot(out$samples[,paste0("lambda_h[",i,",",t,"]")])
    mtext(paste(paste0("lambda_h[",i,",",t,"]"),as.character(atp_players_2000_train$name[i]),t),side = 3,cex = 1.2)
  }
}
dev.off()

pdf("M4_GS_clay_lambda_b_trace_plots.pdf")
par(mfrow = c(3,1))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    coda::traceplot(out$samples[,paste0("lambda_b[",i,",",t,"]")])
    mtext(paste(paste0("lambda_b[",i,",",t,"]"),as.character(atp_players_2000_train$name[i]),t),side = 3,cex = 1.2)
  }
}
dev.off()

pdf("M4_GS_clay_lambda_tilde_trace_plots.pdf")
par(mfrow = c(3,1))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    coda::traceplot(lambda_tilde_mcmc[,paste0("lambda_tilde[",i,",",t,"]")])
    mtext(paste(paste0("lambda_tilde[",i,",",t,"]"),as.character(atp_players_2000_train$name[i]),t),side = 3,cex = 1.2)
  }
}
dev.off()

pdf("M4_GS_clay_p_ij_trace_plots.pdf")
par(mfrow = c(3,1))
for (t in 1:nrow(atp_matches_2000_train)){
  coda::traceplot(out$samples[,paste0("p_ij[",t,"]")])
  mtext(paste(as.character(atp_matches_2000_train$player_i_name[t]),"vs",as.character(atp_matches_2000_train$player_j_name[t])),side = 3,cex = 1.2)
}
dev.off()

pdf("M4_GS_clay_p_ij_density_plots.pdf")
par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  coda::densplot(out$samples[,paste0("p_ij[",t,"]")],ylim = c(0,10000))
  mtext(paste(as.character(atp_matches_2000_train$player_i_name[t]),"vs",as.character(atp_matches_2000_train$player_j_name[t])),side = 3,cex = 0.6)
}
dev.off()

pdf("M4_GS_clay_lambda_b_acf_plots.pdf")
par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    acf(out$sims.list$lambda_b[,i,t],main = paste(as.character(atp_players_2000_train$name[i]),t))
  }
}
dev.off()

pdf("M4_GS_clay_lambda_c_acf_plots.pdf")
par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    acf(out$sims.list$lambda_c[,i,t],main = paste(as.character(atp_players_2000_train$name[i]),t))
  }
}
dev.off()

pdf("M4_GS_clay_lambda_g_acf_plots.pdf")
par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    acf(out$sims.list$lambda_g[,i,t],main = paste(as.character(atp_players_2000_train$name[i]),t))
  }
}
dev.off()

pdf("M4_GS_clay_lambda_h_acf_plots.pdf")
par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    acf(out$sims.list$lambda_h[,i,t],main = paste(as.character(atp_players_2000_train$name[i]),t))
  }
}
dev.off()

pdf("M4_GS_clay_lambda_acf_plots.pdf")
par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    acf(out$sims.list$lambda[,i,t],main = paste(as.character(atp_players_2000_train$name[i]),t))
  }
}
dev.off()

pdf("M4_GS_clay_lambda_tilde_acf_plots.pdf")
par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    acf(lambda_tilde_sims[,i,t],main = paste(as.character(atp_players_2000_train$name[i]),t))
  }
}
dev.off()

pdf("M4_GS_clay_p_ij_acf_plots.pdf")
par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  acf(out$sims.list$p[,i],main=paste0("p[",t,"]"))
  mtext(paste(as.character(atp_matches_2000_train$player_i_name[t]),"vs",as.character(atp_matches_2000_train$player_j_name[t])),side = 3,cex = 0.6)
}
dev.off()

pdf("M4_GS_clay_lambda_b_gelman_plots.pdf")
#par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    param_samples <- out$samples[, paste("lambda_b[",i,",",t,"]",sep = ""), drop = FALSE] 
    coda::gelman.plot(param_samples)
    mtext(as.character(atp_players_2000_train$name[i]),side = 3,cex = 1.2)
  }
}
dev.off()

pdf("M4_GS_clay_lambda_c_gelman_plots.pdf")
#par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    param_samples <- out$samples[, paste("lambda_c[",i,",",t,"]",sep = ""), drop = FALSE] 
    coda::gelman.plot(param_samples)
    mtext(as.character(atp_players_2000_train$name[i]),side = 3,cex = 1.2)
  }
}
dev.off()

pdf("M4_GS_clay_lambda_g_gelman_plots.pdf")
#par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    param_samples <- out$samples[, paste("lambda_g[",i,",",t,"]",sep = ""), drop = FALSE] 
    coda::gelman.plot(param_samples)
    mtext(as.character(atp_players_2000_train$name[i]),side = 3,cex = 1.2)
  }
}
dev.off()

pdf("M4_GS_clay_lambda_h_gelman_plots.pdf")
#par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    param_samples <- out$samples[, paste("lambda_h[",i,",",t,"]",sep = ""), drop = FALSE] 
    coda::gelman.plot(param_samples)
    mtext(as.character(atp_players_2000_train$name[i]),side = 3,cex = 1.2)
  }
}
dev.off()

pdf("M4_GS_clay_lambda_gelman_plots.pdf")
#par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    param_samples <- out$samples[, paste("lambda[",i,",",t,"]",sep = ""), drop = FALSE] 
    coda::gelman.plot(param_samples)
    mtext(as.character(atp_players_2000_train$name[i]),side = 3,cex = 1.2)
  }
}
dev.off()

pdf("M4_GS_clay_lambda_tilde_gelman_plots.pdf")
#par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    param_samples <- lambda_tilde_mcmc[, paste("lambda_tilde[",i,",",t,"]",sep = ""), drop = FALSE] 
    coda::gelman.plot(param_samples)
    mtext(as.character(atp_players_2000_train$name[i]),side = 3,cex = 1.2)
  }
}
dev.off()


pdf("M4_GS_clay_p_ij_gelman_plots.pdf")
#par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  param_samples <- out$samples[, paste("p_ij[",t,"]",sep = ""), drop = FALSE] 
  gelman.plot(param_samples)
  mtext(paste(as.character(atp_matches_2000_train$player_i_name[t]),"vs",as.character(atp_matches_2000_train$player_j_name[t])),side = 3,cex = 1.2)
}
dev.off()


pdf("M4_GS_clay_tau_trace_plots.pdf")
coda::traceplot(out$samples[,"tau_b"],main = "tau_b")
coda::traceplot(out$samples[,"tau_c"],main = "tau_c")
coda::traceplot(out$samples[,"tau_g"],main = "tau_g")
coda::traceplot(out$samples[,"tau_h"],main = "tau_h")
dev.off()

pdf("M4_GS_clay_tau_density_plots.pdf")
coda::densplot(out$samples[,"tau_b"],main = "tau_b")
coda::densplot(out$samples[,"tau_c"],main = "tau_c")
coda::densplot(out$samples[,"tau_g"],main = "tau_g")
coda::densplot(out$samples[,"tau_h"],main = "tau_h")
dev.off()

pdf("M4_GS_clay_tau_acf_plots.pdf")
acf(out$sims.list$tau_b,main = "tau_b")
acf(out$sims.list$tau_c,main = "tau_c")
acf(out$sims.list$tau_g,main = "tau_g")
acf(out$sims.list$tau_h,main = "tau_h")
dev.off()

pdf("M4_GS_clay_tau_gelman_plots.pdf")
param_samples <- out$samples[, "tau_b", drop = FALSE] 
gelman.plot(param_samples)
param_samples <- out$samples[, "tau_c", drop = FALSE] 
gelman.plot(param_samples)
param_samples <- out$samples[, "tau_g", drop = FALSE] 
gelman.plot(param_samples)
param_samples <- out$samples[, "tau_h", drop = FALSE] 
gelman.plot(param_samples)
dev.off()

pdf("M4_GS_clay_betas_trace_plots.pdf")
coda::traceplot(out$samples[,"beta_R"],main = "beta_R")
coda::traceplot(out$samples[,"beta_B"],main = "beta_B")
dev.off()

pdf("M4_GS_clay_betas_density_plots.pdf")
coda::densplot(out$samples[,"beta_R"],main = "beta_R")
coda::densplot(out$samples[,"beta_B"],main = "beta_B")
dev.off()

pdf("M4_GS_clay_betas_acf_plots.pdf")
acf(out$sims.list$beta_R,main = "beta_R")
acf(out$sims.list$beta_B,main = "beta_B")
dev.off()

pdf("M4_GS_clay_betas_gelman_plots.pdf")
param_samples <- out$samples[, "beta_R", drop = FALSE] 
gelman.plot(param_samples)
param_samples <- out$samples[, "beta_B", drop = FALSE] 
gelman.plot(param_samples)
dev.off()


#plotting the traceplots and posteriors for top players

for (i in top_players_ID[1:5]){
  coda::traceplot(out$samples[,paste("lambda_b[",i,",",1,"]",sep = "")],main = paste("lambda_b[",i,",",1,"]",sep = ""))
}

for (i in top_players_ID[1:5]){
  coda::traceplot(out$samples[,paste("lambda_c[",i,",",1,"]",sep = "")],main = paste("lambda_c[",i,",",1,"]",sep = ""))
}

for (i in top_players_ID[1:5]){
  coda::traceplot(out$samples[,paste("lambda_g[",i,",",1,"]",sep = "")],main = paste("lambda_g[",i,",",1,"]",sep = ""))
}

for (i in top_players_ID[1:5]){
  coda::traceplot(out$samples[,paste("lambda_h[",i,",",1,"]",sep = "")],main = paste("lambda_h[",i,",",1,"]",sep = ""))
}

for (i in top_players_ID[1:5]){
  coda::traceplot(out$samples[,paste("lambda[",i,",",1,"]",sep = "")],main = paste("lambda[",i,",",1,"]",sep = ""))
}

for (i in top_players_ID[1:5]){
  coda::traceplot(lambda_tilde_mcmc[,paste("lambda_tilde[",i,",",1,"]",sep = "")],main = paste("lambda_tilde[",i,",",1,"]",sep = "")) #problems converging
}

for (i in top_players_ID[1:5]){
  coda::densplot(out$samples[,paste("lambda_b[",i,",",1,"]",sep = "")],main = paste("lambda_b[",i,",",1,"]",sep = ""))
}

for (i in top_players_ID[1:5]){
  coda::densplot(out$samples[,paste("lambda_c[",i,",",1,"]",sep = "")],main = paste("lambda_c[",i,",",1,"]",sep = ""))
}

for (i in top_players_ID[1:5]){
  coda::densplot(out$samples[,paste("lambda_g[",i,",",1,"]",sep = "")],main = paste("lambda_g[",i,",",1,"]",sep = ""))
}

for (i in top_players_ID[1:5]){
  coda::densplot(out$samples[,paste("lambda_h[",i,",",1,"]",sep = "")],main = paste("lambda_h[",i,",",1,"]",sep = ""))
}

for (i in top_players_ID[1:5]){
  coda::densplot(out$samples[,paste("lambda[",i,",",1,"]",sep = "")],main = paste("lambda[",i,",",1,"]",sep = ""))
}

for (i in top_players_ID[1:5]){
  coda::densplot(lambda_tilde_mcmc[,paste("lambda_tilde[",i,",",1,"]",sep = "")],main = paste("lambda_tilde[",i,",",1,"]",sep = "")) #problems converging
}


for (i in 1:length(top_players_ID[1:5])){
  acf(out$sims.list$lambda[,top_players_ID[i],2],main = paste("ACF for Posterior Strength of",top_player_names[i]))
}

for (i in 1:length(top_players_ID)){
  acf(out$sims.list$lambda_tilde[,top_players_ID[i],2],main = paste("ACF for Posterior Strength of",top_player_names[i]))
}

#checking the required burn ins using the Gelman plot
samples <- out$samples
param_samples <- samples[, paste("lambda_tilde[",top_players_ID[1],",",1,"]",sep = ""), drop = FALSE] 
gelman.plot(param_samples)

#Repeating for p_ij
jagshelper::trace_jags(out,p = "p_ij[2]") #choose the match
acf(out$sims.list$p_ij[,2])

#Getting the probabilities of matches for some top players - not used

#predicting outcomes based on mean probability > 0.5
y_pred <- as.integer(1*(out$mean$p_ij > 0.5))

#gets the times the top player played
play_times_top_player <- atp_matches_2000_train$Time[which(atp_matches_2000_train$winner_id == top_players_ID[1] | atp_matches_2000_train$loser_id == top_players_ID[1])] 

#constructs the dataframe of matches of all matches of the top player
top_player_win_probs <- data.frame(Time = play_times_top_player,
                                   player_i_name = atp_matches_2000_train$player_i_name[play_times_top_player],
                                   player_j_name = atp_matches_2000_train$player_j_name[play_times_top_player],
                                   p_ij = out$mean$p_ij[play_times_top_player],
                                   outcome = atp_matches_2000_train$y[play_times_top_player],
                                   y_pred = y_pred[play_times_top_player])

#refactors the variables
top_player_win_probs$outcome <- as.factor(top_player_win_probs$outcome)
top_player_win_probs$y_pred <- as.factor(top_player_win_probs$y_pred)

#constructs the confusion matrix
confusionMatrix(data = top_player_win_probs$y_pred,reference = top_player_win_probs$outcome)


#Repeats for second best player
#gets the times the top player played
play_times_second_player <- atp_matches_2000_train$Time[which(atp_matches_2000_train$winner_id == top_players_ID[2] | atp_matches_2000_train$loser_id == top_players_ID[2])] 

#constructs the dataframe of matches of all matches of the top player
second_player_win_probs <- data.frame(Time = play_times_second_player,
                                   player_i_name = atp_matches_2000_train$player_i_name[play_times_second_player],
                                   player_j_name = atp_matches_2000_train$player_j_name[play_times_second_player],
                                   p_ij = out$mean$p_ij[play_times_second_player],
                                   outcome = atp_matches_2000_train$y[play_times_second_player],
                                   y_pred = y_pred[play_times_second_player])

#refactors the variables
second_player_win_probs$outcome <- as.factor(second_player_win_probs$outcome)
second_player_win_probs$y_pred <- as.factor(second_player_win_probs$y_pred)

#constructs the confusion matrix
confusionMatrix(data = second_player_win_probs$y_pred,reference = second_player_win_probs$outcome)

#Repeats for third best player
#gets the times the top player played
play_times_third_player <- atp_matches_2000_train$Time[which(atp_matches_2000_train$winner_id == top_players_ID[3] | atp_matches_2000_train$loser_id == top_players_ID[3])] 

#constructs the dataframe of matches of all matches of the top player
third_player_win_probs <- data.frame(Time = play_times_third_player,
                                      player_i_name = atp_matches_2000_train$player_i_name[play_times_third_player],
                                      player_j_name = atp_matches_2000_train$player_j_name[play_times_third_player],
                                      p_ij = out$mean$p_ij[play_times_third_player],
                                      outcome = atp_matches_2000_train$y[play_times_third_player],
                                      y_pred = y_pred[play_times_third_player])

#refactors the variables
third_player_win_probs$outcome <- as.factor(third_player_win_probs$outcome)
third_player_win_probs$y_pred <- as.factor(third_player_win_probs$y_pred)

#constructs the confusion matrix
confusionMatrix(data = third_player_win_probs$y_pred,reference = third_player_win_probs$outcome)

#we can do the above for any player, just edit the indices of the players


#====================checking convergence and finding which player strengths / probabilities did not converge========================= -not used
#rhat_lambda <- as.data.frame(out$Rhat$lambda)
#not_converged_player_strengths <- which(rhat_lambda > 1.1)

#rhat_lambda[not_converged_player_strengths,1] #getting rhat values for those who did not converge

#for (i in not_converged_player_strengths){ #plotting trace plots for those who did not converge
#  jagsUI::traceplot(out,parameters = paste("lambda[",i,"]",sep = ""))
#}

#checking whether the above players won any matches
#for (i in not_converged_player_strengths){
#  print(which(atp_matches_2000_train$winner_id == i))
#}

#these players therefore did not win any matches, which corresponds to the issues in classical BT, where strengths tend to +- infinity. In the Bayesian case, we get very low abilities and on top of that, non-convergence. 

#checking acf plots for these parameters
#for (i in not_converged_player_strengths){
#  acf(out$sims.list$lambda[,i])
#}

#===============repeating for p_ij=================#
rhat_p <- as.data.frame(out$Rhat$p) #saving the rhat values
not_converged_prob <- which(rhat_p > 1.1) #finding those matches which did not converge

rhat_p[not_converged_prob,1] #getting rhat values for those who did not converge

#plotting traceplots for those matches which did not converge
for (i in not_converged_prob){
  jagsUI::traceplot(out,parameters = paste("p_ij[",i,"]",sep = ""))
}

############################################################################################################################
#Posterior predictive checks (using bayesplot)
############################################################################################################################

N <- nrow(out$sims.list$p)
n <- length(model.data$y)
K <- nrow(atp_players_2000_train)

#simulate from the posterior predictive distribution
Y_rep <- mat.or.vec(N,n)

for (t in 1:n){
  Y_rep[,t] <- rbinom(n = N,p = out$sims.list$p[,t],size = 1) #getting samples of the posterior predictive distribution for each match and match-win probability in the posterior sample
}

#Posterior Predictive Checks: Plots


ppc_hist(model.data$y,Y_rep[1:5,],binwidth = 0.1) #histograms
ppc_bars(model.data$y,Y_rep,width = 0.1,prob = 0.95,linewidth = 1) #bar charts

ppc_stat(model.data$y,yrep = Y_rep,stat = "mean") #distribution of mean of replicated samples compared to the true mean
ppc_stat(model.data$y,yrep = Y_rep,stat = "var") #distribution of variance of replicated samples compared to the true variance

ppc_stat_2d(model.data$y,yrep = Y_rep,stat = c("mean","var")) #combining the above two plots


#formulating the chi square measure of discrepancy
chi_disc <- function(y, p) {
  # Perturb p values that are 0 or 1
  p <- ifelse(p == 0, 1e-6, p) # Replace 0 with a small positive value
  p <- ifelse(p == 1, 1 - 1e-6, p) # Replace 1 with a value slightly less than 1
  
  # Calculate the chi-squared measure of discrepancy
  return(sum((y - p)^2 / (p * (1 - p))))
}

#Finding the chi square measure of discrepancy for the observed data
chi_disc_true <- mat.or.vec(N,1)
for (nu in 1:N){
  chi_disc_true[nu] <- chi_disc(model.data$y,out$sims.list$p[nu,])  
}

#Finding the chi square measure of discrepancy for the replicated data
chi_disc_rep <- mat.or.vec(N,1)
for (nu in 1:N){
  chi_disc_rep[nu] <- chi_disc(Y_rep[nu,],out$sims.list$p[nu,])  
}

#plotting the scatter plot 
plot(chi_disc_true,chi_disc_rep)

#counting how many of the replicated values exceed the true
sum(chi_disc_rep>=chi_disc_true)

#estimating the posterior predictive p-value
p_b_hat <- sum(chi_disc_rep>=chi_disc_true)/N

############################################################################################################################
#Distribution of the BIC
############################################################################################################################

BIC <- out$sims.list$deviance + (4*K*n+6)*log(n)
hist(BIC)

save(BIC,file = "M4_GS_BIC.RData")