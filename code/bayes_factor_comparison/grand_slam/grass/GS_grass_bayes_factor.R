#======== This script file calculates the Bayes Factor for the grass surface in Grand Slam Tour data set =========

#clearing environment 
rm(list = ls())

#=== loading all BICs for models M1,M2,M3,M4 ===

#loading M1
load("~/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Analysing_model_fit_outputs/fixed_time_bt/grand_slam/grand_slam_grass/M1_GS_grass_BIC.RData")
M1_BIC <- BIC

#loading M2
load("~/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Analysing_model_fit_outputs/gorgi_noreg_nosurf/grand_slam/grand_slam_grass/M2_GS_grass_BIC.RData")
M2_BIC <- BIC

#loading M3
load("~/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Analysing_model_fit_outputs/gorgi_reg_nosurf/grand_slam/grand_slam_grass/M3_GS_grass_BIC.RData")
M3_BIC <- BIC

#loading M4 (not a fair comparison since data is different)
load("~/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Analysing_model_fit_outputs/gorgi_reg_surf/grand_slam/M4_GS_BIC.RData")
M4_BIC <- BIC

#Obtaining B_12
bf_12 <- exp(0.5*(M2_BIC-M1_BIC))
log10(1/bf_12) #used to get an overall measure of evidence

#Obtaining B_13
bf_13 <- exp(0.5*(M3_BIC-M1_BIC))
log10(1/bf_13) #used to get an overall measure of evidence

#Obtaining B_14
bf_14 <- exp(0.5*(M4_BIC-M1_BIC))
log10(1/bf_14) #used to get an overall measure of evidence

#Obtaining B_23
bf_23 <- exp(0.5*(M3_BIC-M2_BIC))
mean(bf_23)
mean(log10(1/bf_23)) #used to get an overall measure of evidence
hist(log10(1/bf_23))

#Obtaining B_24
bf_24 <- exp(0.5*(M4_BIC-M2_BIC))
log10(1/bf_24) #used to get an overall measure of evidence

#Obtaining B_34
bf_34 <- exp(0.5*(M4_BIC-M3_BIC))
log10(1/bf_34) #used to get an overall measure of evidence





