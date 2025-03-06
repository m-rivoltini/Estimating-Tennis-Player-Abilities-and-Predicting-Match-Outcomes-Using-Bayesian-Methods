#=========== Analysing the output of M1 fitted on the Masters tour combining all surfaces ===========

#clearing environments and loading packages
rm(list=ls())
library(jagshelper)
library(jagsUI)
library(coda)
library(ggplot2)
library(bayesplot)

#setting working directory
setwd("~/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Analysing_model_fit_outputs/fixed_time_bt/master/master_allsurf")

#loading all files and getting computation times for each run
load("/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/fixed_time_bt/master/master_allsurf/BT_fixed_time_out_MS_allsurf.RData")
run_time

load("/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/fixed_time_bt/master/master_allsurf/BT_fixed_time_out_MS_allsurf_with_extra_burnin_small_iter_no_thin.RData")
run_time_2

load("/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/fixed_time_bt/master/master_allsurf/BT_fixed_time_out_MS_allsurf_with_extra_burnin.RData")
run_time_2

load("/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/fixed_time_bt/master/master_allsurf/BT_fixed_time_out_MS_allsurf_with_extra_burnin_2.RData")
run_time_2

#loading the data sets
atp_matches_2000 <- read.csv2("/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/fixed_time_bt/master/master_allsurf/atp_matches_2000_train_MS_allsurf.csv")

atp_players_2000 <- read.csv2("/Users/mishayelrivoltini/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Model_fitting_home_computer/fixed_time_bt/master/master_allsurf/atp_players_2000_train_MS_allsurf.csv")

#ordering the players by player ID
atp_players_2000 <- atp_players_2000[order(atp_players_2000$player_id),]

#======================================================================================================#
#==============================================Analysis================================================#
#======================================================================================================#

#summary statistics of output
summary(out) 

#Plotting Rhats
plotRhats(out,p = "lambda")
plotRhats(out,p = "p")

#saving the summaries
summaries <- as.data.frame(out$summary) 

#Finding the player names corresponding to the strengths and the names of players i_t vs j_t (match-ups)
player_names_opponents <- c(atp_players_2000$name,paste(atp_matches_2000$player_i_name,"vs",atp_matches_2000$player_j_name),"NA") #NA here is for the deviance
summaries$names <- player_names_opponents #appending player names and match-ups
summaries <- subset(summaries,select = c("names","mean","sd","2.5%","97.5%","Rhat","n.eff")) #ordering the columns

#saving the summaries
write.csv(summaries,file = "BT_MS_allsurf_summaries.csv")

#gives the run_time of the model.
run_time 
run_time_2

#exporting the posterior means of the abilities and match-win probabilities
lambda <- as.data.frame(out$mean$lambda) #means of lambda 
p <- as.data.frame(out$mean$p) #means of p_ij's

#===================showing some plots and convergence diagnostics for the top 15 players============================#

#getting the IDs of the top 15 players with the highest posterior mean abilities
top_15_players_ID <- order(lambda$`out$mean$lambda`,decreasing = TRUE)[1:15]

#getting the winner names from the ids
top_15_player_names <- atp_matches_2000$winner_name[match(top_15_players_ID, atp_matches_2000$winner_id)] 

#saving the variable names lambda[i] for i= top player IDs
top_15_lambda_names <- mat.or.vec(length(top_15_players_ID),1) 
for (i in 1:length(top_15_players_ID)){
  top_15_lambda_names[i] <- paste("lambda[",top_15_players_ID[i],"]",sep = "")
}

#getting the posterior samples of the abilities for the top 15 players
lambda_sims_top_15 <- as.data.frame(out$sims.list$lambda)[,top_15_players_ID]
colnames(lambda_sims_top_15) <- top_15_player_names #changing the column names to match the player names

#plotting the mean and hpd intervals for the top 15 players
mcmc_intervals(lambda_sims_top_15,point_est = "mean",rhat = out$Rhat$lambda[top_15_players_ID],prob_outer = 0.95,prob = 0.5)

#plotting the traceplots, posteriors, acfs and gelman plots for all players
pdf("M1_MS_allsurf_lambda_trace_plots.pdf")
par(mfrow = c(3,1))
for (i in 1:nrow(atp_players_2000)){
  jagsUI::traceplot(out,parameters  = paste("lambda[",i,"]",sep = ""))
  mtext(as.character(atp_players_2000$name[i]),side = 3,cex = 1.2)
}
dev.off()



pdf("M1_MS_allsurf_lambda_density_plots.pdf")
par(mfrow = c(3,3))
for (i in 1:nrow(atp_players_2000)){
  jagsUI::densityplot(out,parameters  = paste("lambda[",i,"]",sep = ""))
  mtext(as.character(atp_players_2000$name[i]),side = 3,cex = 0.6)
}
dev.off()

#plotting the traceplots and posteriors for top players
pdf("M1_MS_allsurf_p_ij_trace_plots.pdf")
par(mfrow = c(3,1))
for (i in 1:nrow(atp_matches_2000)){
  jagsUI::traceplot(out,parameters  = paste("p[",i,"]",sep = ""))
  mtext(paste(as.character(atp_matches_2000$player_i_name[i]),"vs",as.character(atp_matches_2000$player_j_name[i])),side = 3,cex = 1.2)
}
dev.off()

pdf("M1_MS_allsurf_p_ij_density_plots.pdf")
par(mfrow = c(3,3))
for (i in 1:nrow(atp_matches_2000)){
  jagsUI::densityplot(out,parameters  = paste("p[",i,"]",sep = ""))
  mtext(paste(as.character(atp_matches_2000$player_i_name[i]),"vs",as.character(atp_matches_2000$player_j_name[i])),side = 3,cex = 0.6)
}
dev.off()

pdf("M1_MS_allsurf_lambda_acf_plots.pdf")
par(mfrow = c(3,3))
for (i in 1:nrow(atp_players_2000)){
  acf(out$sims.list$lambda[,i],main = as.character(atp_players_2000$name[i]))
}
dev.off()

pdf("M1_MS_allsurf_p_ij_acf_plots.pdf")
par(mfrow = c(3,3))
for (i in 1:nrow(atp_matches_2000)){
  acf(out$sims.list$p[,i],main=paste0("p[",i,"]"))
  mtext(paste(as.character(atp_matches_2000$player_i_name[i]),"vs",as.character(atp_matches_2000$player_j_name[i])),side = 3,cex = 0.6)
}
dev.off()

pdf("M1_MS_allsurf_lambda_gelman_plots.pdf")
#par(mfrow = c(3,3))
for (i in 1:nrow(atp_players_2000)){
  param_samples <- out$samples[, paste("lambda[",i,"]",sep = ""), drop = FALSE] 
  gelman.plot(param_samples)
  mtext(as.character(atp_players_2000$name[i]),side = 3,cex = 1.2)
}
dev.off()

pdf("M1_MS_allsurf_p_ij_gelman_plots.pdf")
#par(mfrow = c(3,3))
for (i in 1:nrow(atp_matches_2000)){
  param_samples <- out$samples[, paste("p[",i,"]",sep = ""), drop = FALSE] 
  gelman.plot(param_samples)
  mtext(paste(as.character(atp_matches_2000$player_i_name[i]),"vs",as.character(atp_matches_2000$player_j_name[i])),side = 3,cex = 1.2)
}
dev.off()

#plotting the traceplots and posteriors for top players
for (i in top_15_players_ID){
  jagshelper::trace_jags(out,p = paste("lambda[",i,"]",sep = ""))
}
for (i in top_15_players_ID){
  jagshelper::chaindens_jags(out,p = paste("lambda[",i,"]",sep = ""))
}

for (i in 1:length(top_15_players_ID)){
  acf(out$sims.list$lambda[,top_15_players_ID[i]],main = paste("ACF for Posterior Strength of",top_15_player_names[i]))
}

samples <- out$samples
for (k in 1:15){
  param_samples <- samples[, paste("lambda[",top_15_players_ID[k],"]",sep = ""), drop = FALSE] 
  gelman.plot(param_samples)
}

#====================checking convergence and finding which player strengths / probabilities did not converge=========================
rhat_lambda <- as.data.frame(out$Rhat$lambda)
not_converged_player_strengths <- which(rhat_lambda > 1.1) #note how these players are the ones who lost all their matches

rhat_lambda[not_converged_player_strengths,1] #getting rhat values for those who did not converge

for (i in not_converged_player_strengths){ #plotting trace plots for those who did not converge
  jagsUI::traceplot(out,parameters = paste("lambda[",i,"]",sep = ""))
}

#checking whether the above players won any matches
for (i in not_converged_player_strengths){
  print(which(atp_matches_2000$winner_id == i))
}

#these players therefore did not win any matches, which corresponds to the issues in classical BT, where strengths tend to +- infinity. In the Bayesian case, we get very low abilities and on top of that, non-convergence. 

#checking acf plots for these parameters
for (i in not_converged_player_strengths){
  acf(out$sims.list$lambda[,i])
}


#===============repeating for p_ij=================#
rhat_p <- as.data.frame(out$Rhat$p) #saving the rhat values
not_converged_prob <- which(rhat_p > 1.1) #finding those matches which did not converge

rhat_p[not_converged_prob,1] #getting rhat values for those who did not converge

#plotting traceplots for those matches which did not converge
for (i in not_converged_prob){
  jagsUI::traceplot(out,parameters = paste("p[",i,"]",sep = ""))
}

############################################################################################################################
#Posterior predictive checks (using bayesplot)
############################################################################################################################


N <- nrow(out$sims.list$p)
n <- length(model.data$y)
K <- nrow(atp_players_2000)

#simulate from the posterior predictive distribution
Y_rep <- mat.or.vec(N,n)

for (t in 1:n){
  Y_rep[,t] <- rbern(N,out$sims.list$p[,t]) #getting samples of the posterior predictive distribution for each match and match-win probability in the posterior sample
}

#Posterior Predictive Checks - Plots

ppc_hist(model.data$y,Y_rep[1:5,],binwidth = 0.1) #histogram
ppc_bars(model.data$y,Y_rep,width = 0.1,prob = 0.95,linewidth = 1) #bar charts

ppc_stat(model.data$y,yrep = Y_rep,stat = "mean") #distribution of means of Y_rep compared to the true mean
ppc_stat(model.data$y,yrep = Y_rep,stat = "var") #distribution of variances of Y_rep compared to the true variance

ppc_stat_2d(model.data$y,yrep = Y_rep,stat = c("mean","var")) #combining the above two plots


#defining the chi squared measure of discrepancy.
chi_disc <- function(y, p) {
  # Perturb p values that are 0 or 1 to avoid division by zero
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

BIC <- out$sims.list$deviance + K*log(n)
hist(BIC)

save(BIC,file = "M1_MS_allsurf_BIC")