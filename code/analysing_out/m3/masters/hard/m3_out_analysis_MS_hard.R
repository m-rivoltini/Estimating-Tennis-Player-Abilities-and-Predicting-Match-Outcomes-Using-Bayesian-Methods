# ================= Analysing outputs of M3 applied to the hard data set for Masters Tour ===========

#clearing environment and loading packages
rm(list=ls())
library(jagshelper)
library(jagsUI)
library(coda)
library(HDInterval)
library(caret)

#setting working directory
setwd("~/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Analysing_model_fit_outputs/gorgi_reg_nosurf/master/master_hard")

#Remember the strategy we used. Ran small number of iterations on home computer to get a high enough burn-in, and then run on supercomputer to increase the sample size.

#Done on home computer
load("/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/gorgi_reg_nosurf/master/master_hard/gorgi_model_reg_nosurf_output_MS_hard_reduced_stand_pred_noht_noage_out.RData")
run_time

load("/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/gorgi_reg_nosurf/master/master_hard/gorgi_reg_nosurf_MS_hard_reduced_with_extra_burnin_stand_pred_noht_noage.RData")
run_time

load("/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/gorgi_reg_nosurf/master/master_hard/gorgi_reg_nosurf_MS_hard_reduced_with_extra_burnin_stand_pred_noht_noage_2.RData")
run_time

#Done on super computer

load("/Volumes/SanDisk SSD/Model_fitting_V2/gorgi_reg_nosurf/master/master_hard/gorgi_reg_nosurf_MS_hard_with_extra_burnin_stand_pred_noht_noage_reduced_2.RData")
run_time

#importing data 
atp_matches_2000_train <- read.csv2("/Volumes/SanDisk SSD/Model_fitting_V2/gorgi_reg_nosurf/master/master_hard/atp_matches_2000_train_MS_hard_reduced_2.csv")

atp_players_2000_train <- read.csv2("/Volumes/SanDisk SSD/Model_fitting_V2/gorgi_reg_nosurf/master/master_hard/atp_players_2000_train_MS_hard_reduced_2.csv")

#ordering the player IDs
atp_players_2000_train <- atp_players_2000_train[order(atp_players_2000_train$player_id),]

#======================================================================================================#
#==============================================Analysis================================================#
#======================================================================================================#

#summary statistics of output - note that lambda_tilde obtained from here is incorrect, so we adjust it
summary(out) 

#checking the maximum rhat values for the parameters
max(out$Rhat$lambda)
max(out$Rhat$lambda_tilde)
max(out$Rhat$p_ij)
length(which(out$Rhat$p_ij>1.1))

max(out$Rhat$tau)
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
chain_1 <- mcmc(lambda_tilde_sims_flat[1:1000,,])
chain_2 <- mcmc(lambda_tilde_sims_flat[1001:2000,,])
chain_3 <- mcmc(lambda_tilde_sims_flat[2001:3000,,])

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
player_names_opponents <- c(names_times,paste(atp_matches_2000_train$player_i_name,"vs",atp_matches_2000_train$player_j_name),"beta_R","beta_B",paste("~",names_times),"tau","NA") #NA here is for the deviance

summaries$names <- player_names_opponents #appending names and match ups
summaries <- subset(summaries,select = c("names","mean","sd","2.5%","97.5%","Rhat","n.eff")) #reordering columns

#replace incorrect summaries of lambda_tilde
start_row <- which(rownames(summaries) == "lambda_tilde[1,1]")
end_row <- which(rownames(summaries) == paste0("lambda_tilde[",nrow(atp_players_2000_train),",",nrow(atp_matches_2000_train),"]"))
summaries[start_row:end_row,2:7] <- lambda_tilde_summary

#saving summaries
write.csv(summaries,file = "M3_MS_hard_summaries.csv")

#exporting mean abilities and finding maximum and average abilities
lambda <- as.data.frame(out$mean$lambda) #means of lambda 
lambda$max_strength <- apply(lambda,1,max)
lambda$av_strength <- apply(lambda,1,mean)

lambda_tilde <- as.data.frame(lambda_tilde) #means of lambda_tilde
lambda_tilde$max_strength <- apply(lambda_tilde,1,max)
lambda_tilde$av_strength <- apply(lambda_tilde,1,mean)


p <- as.data.frame(out$mean$p) #means of p_ij's (outputs seem to make sense)

#===================showing some plots and convergence diagnostics for the top players============================#

#ordering player IDs based on the average posterior means across the entire period
top_players_ID <- order(lambda_tilde$av_strength,decreasing = TRUE)#[1:10]

#getting the winner names from the ids
top_player_names <- atp_players_2000_train$name[match(top_players_ID, atp_players_2000_train$player_id)] 

#Plotting evolution of player abilities for top 4 players
for (k in 1:5){
  plot(1:(ncol(lambda_tilde)-2),lambda_tilde[top_players_ID[k],1:(ncol(lambda_tilde)-2)],type="l",xlab = "Time",ylab = "Complete Ability",main = paste("Evolution of Ability for",top_player_names[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda_tilde[top_players_ID[k],1:(ncol(lambda_tilde)-2)])) != 0)
  change_times <- (1:(ncol(lambda_tilde)-2))[change_points]
  points(change_times,lambda_tilde[top_players_ID[k],change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}

pdf("M3_GS_clay_seq_plots_lambda_tilde_top_players_ordered.pdf")
par(mfrow = c(3,1))
for (k in 1:nrow(atp_players_2000_train)){
  plot(1:(ncol(lambda_tilde)-2),lambda_tilde[top_players_ID[k],1:(ncol(lambda_tilde)-2)],type="l",xlab = "Time",ylab = "Complete Ability",main = paste("Evolution of Ability for",top_player_names[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda_tilde[top_players_ID[k],1:(ncol(lambda_tilde)-2)])) != 0)
  change_times <- (1:(ncol(lambda_tilde)-2))[change_points]
  points(change_times,lambda_tilde[top_players_ID[k],change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}
dev.off()

pdf("M3_GS_clay_lambda_tilde_seq_plots.pdf")
par(mfrow = c(3,1))
for (k in 1:nrow(atp_players_2000_train)){
  plot(1:(ncol(lambda_tilde)-2),lambda_tilde[k,1:(ncol(lambda_tilde)-2)],type="l",xlab = "Time",ylab = "Complete Ability",main = paste("Evolution of Ability for",atp_players_2000_train$name[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda_tilde[k,1:(ncol(lambda_tilde)-2)])) != 0)
  change_times <- (1:(ncol(lambda_tilde)-2))[change_points]
  points(change_times,lambda_tilde[k,change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}
dev.off()


pdf("M3_MS_hard_seq_plots_top_players_ordered.pdf")
par(mfrow = c(3,1))
for (k in 1:nrow(atp_players_2000_train)){
  plot(1:(ncol(lambda)-2),lambda[top_players_ID[k],1:(ncol(lambda)-2)],type="l",xlab = "Time",ylab = "Ability",main = paste("Evolution of Ability for",top_player_names[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda[top_players_ID[k],1:(ncol(lambda)-2)])) != 0)
  change_times <- (1:(ncol(lambda)-2))[change_points]
  points(change_times,lambda[top_players_ID[k],change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}
dev.off()

pdf("M3_MS_hard_seq_plots.pdf")
par(mfrow = c(3,1))
for (k in 1:nrow(atp_players_2000_train)){
  plot(1:(ncol(lambda)-2),lambda[k,1:(ncol(lambda)-2)],type="l",xlab = "Time",ylab = "Ability",main = paste("Evolution of Ability for",atp_players_2000_train$name[k]))
  change_points <- c(TRUE, diff(as.numeric(lambda[k,1:(ncol(lambda)-2)])) != 0)
  change_times <- (1:(ncol(lambda)-2))[change_points]
  points(change_times,lambda[k,change_times], bg='tomato', pch=21, cex=0.5, lwd=0.1)
}
dev.off()


#plotting the traceplots, posteriors, acfs and gelman plots for all players

pdf("M3_MS_hard_lambda_trace_plots.pdf")
par(mfrow = c(3,1))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    coda::traceplot(out$samples[,paste0("lambda[",i,",",t,"]")])
    mtext(paste(paste0("lambda[",i,",",t,"]"),as.character(atp_players_2000_train$name[i]),t),side = 3,cex = 1.2)
  }
}
dev.off()


pdf("M3_MS_hard_lambda_density_plots.pdf")
par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    coda::densplot(out$samples[,paste0("lambda[",i,",",t,"]")])
    mtext(paste(paste0("lambda[",i,",",t,"]")," ",as.character(atp_players_2000_train$name[i]),"at time",t),side = 3,cex = 0.6)
  }
}
dev.off()

pdf("M3_MS_hard_lambda_tilde_trace_plots.pdf")
par(mfrow = c(3,1))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    coda::traceplot(out$samples[,paste0("lambda_tilde[",i,",",t,"]")])
    mtext(paste(paste0("lambda[",i,",",t,"]"),as.character(atp_players_2000_train$name[i]),t),side = 3,cex = 1.2)
  }
}
dev.off()


pdf("M3_MS_hard_lambda_tilde_density_plots.pdf")
par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    coda::densplot(out$samples[,paste0("lambda_tilde[",i,",",t,"]")])
    mtext(paste(paste0("lambda[",i,",",t,"]")," ",as.character(atp_players_2000_train$name[i]),"at time",t),side = 3,cex = 0.6)
  }
}
dev.off()

#plotting the traceplots and posteriors for top players
pdf("M3_MS_hard_p_ij_trace_plots.pdf")
par(mfrow = c(3,1))
for (t in 1:nrow(atp_matches_2000_train)){
  coda::traceplot(out$samples[,paste0("p_ij[",t,"]")])
  mtext(paste(as.character(atp_matches_2000_train$player_i_name[t]),"vs",as.character(atp_matches_2000_train$player_j_name[t])),side = 3,cex = 1.2)
}
dev.off()

pdf("M3_MS_hard_p_ij_density_plots.pdf")
par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  coda::densplot(out$samples[,paste0("p_ij[",t,"]")])
  mtext(paste(as.character(atp_matches_2000_train$player_i_name[t]),"vs",as.character(atp_matches_2000_train$player_j_name[t])),side = 3,cex = 0.6)
}
dev.off()

pdf("M3_MS_hard_lambda_acf_plots.pdf")
par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    acf(out$sims.list$lambda[,i,t],main = paste(as.character(atp_players_2000_train$name[i]),t))
  }
}
dev.off()

pdf("M3_MS_hard_lambda_tilde_acf_plots.pdf")
par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    acf(out$sims.list$lambda_tilde[,i,t],main = paste(as.character(atp_players_2000_train$name[i]),t))
  }
}
dev.off()

pdf("M3_MS_hard_p_ij_acf_plots.pdf")
par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  acf(out$sims.list$p[,i],main=paste0("p[",t,"]"))
  mtext(paste(as.character(atp_matches_2000_train$player_i_name[t]),"vs",as.character(atp_matches_2000_train$player_j_name[t])),side = 3,cex = 0.6)
}
dev.off()

pdf("M3_MS_hard_lambda_gelman_plots.pdf")
#par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    param_samples <- out$samples[, paste("lambda[",i,",",t,"]",sep = ""), drop = FALSE] 
    coda::gelman.plot(param_samples)
    mtext(as.character(atp_players_2000_train$name[i]),side = 3,cex = 1.2)
  }
}
dev.off()

pdf("M3_MS_hard_lambda_tilde_gelman_plots.pdf")
#par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  for (i in 1:nrow(atp_players_2000_train)){
    param_samples <- out$samples[, paste("lambda_tilde[",i,",",t,"]",sep = ""), drop = FALSE] 
    coda::gelman.plot(param_samples)
    mtext(as.character(atp_players_2000_train$name[i]),side = 3,cex = 1.2)
  }
}
dev.off()

pdf("M3_MS_hard_p_ij_gelman_plots.pdf")
#par(mfrow = c(3,3))
for (t in 1:nrow(atp_matches_2000_train)){
  param_samples <- out$samples[, paste("p_ij[",t,"]",sep = ""), drop = FALSE] 
  gelman.plot(param_samples)
  mtext(paste(as.character(atp_matches_2000_train$player_i_name[t]),"vs",as.character(atp_matches_2000_train$player_j_name[t])),side = 3,cex = 1.2)
}
dev.off()

pdf("M3_MS_hard_tau_trace_plots.pdf")
coda::traceplot(out$samples[,"tau"])
dev.off()

pdf("M3_MS_hard_tau_density_plots.pdf")
coda::densplot(out$samples[,"tau"])
dev.off()

pdf("M3_MS_hard_tau_acf_plots.pdf")
acf(out$sims.list$tau,main = "tau")
dev.off()

pdf("M3_MS_hard_tau_gelman_plots.pdf")
param_samples <- out$samples[, "tau", drop = FALSE] 
gelman.plot(param_samples)
dev.off()

pdf("M3_MS_hard_betas_trace_plots.pdf")
coda::traceplot(out$samples[,"beta_R"])
coda::traceplot(out$samples[,"beta_B"])
dev.off()

pdf("M3_MS_hard_betas_density_plots.pdf")
coda::densplot(out$samples[,"beta_R"])
coda::densplot(out$samples[,"beta_B"])
dev.off()

pdf("M3_MS_hard_betas_acf_plots.pdf")
acf(out$sims.list$beta_R,main = "beta_R")
acf(out$sims.list$beta_B,main = "beta_B")
dev.off()

pdf("M3_MS_hard_betas_gelman_plots.pdf")
param_samples <- out$samples[, "beta_R", drop = FALSE] 
gelman.plot(param_samples)
param_samples <- out$samples[, "beta_B", drop = FALSE] 
gelman.plot(param_samples)
dev.off()

#plotting the traceplots and posteriors for top players
for (i in top_players_ID[1:5]){
  coda::traceplot(out$samples[,paste("lambda_tilde[",i,",",1,"]",sep = "")],main = paste("lambda_tilde[",i,",",1,"]",sep = "")) #problems converging
}
for (i in top_players_ID[1:5]){
  coda::densplot(out$samples[,paste("lambda_tilde[",i,",",1,"]",sep = "")],main = paste("lambda_tilde[",i,",",1,"]",sep = "")) #problems converging
}

for (i in 1:length(top_players_ID[1:5])){
  acf(out$sims.list$lambda[,top_players_ID[i],2],main = paste("ACF for Posterior Strength of",top_player_names[i]))
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


#====================checking convergence and finding which player strengths / probabilities did not converge========================= - not used
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
  # Perturb p values that are 0 or 1 to avoid dividing by 0
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
count(chi_disc_rep>=chi_disc_true)

#estimating the posterior predictive p-value
p_b_hat <- count(chi_disc_rep>=chi_disc_true)/N

############################################################################################################################
#Distribution of the BIC
############################################################################################################################

BIC <- out$sims.list$deviance + (K*n+3)*log(n)
hist(BIC)