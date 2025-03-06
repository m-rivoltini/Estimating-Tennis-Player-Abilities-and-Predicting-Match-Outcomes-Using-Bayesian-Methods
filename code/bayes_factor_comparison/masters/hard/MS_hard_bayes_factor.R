#======== This script file calculates the Bayes Factor for the hard surface in Masters Tour data set =========

#clearing environment 
rm(list = ls())

#=== loading all BICs for models M1,M2,M3,M4 ===

#loading M1
load("~/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Analysing_model_fit_outputs/fixed_time_bt/master/master_hard/M1_MS_hard_BIC.RData")
M1_BIC <- BIC

#loading M2
load("~/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Analysing_model_fit_outputs/gorgi_noreg_nosurf/master/master_hard/M2_MS_hard_BIC.RData")
M2_BIC <- BIC

#loading M3
load("~/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Analysing_model_fit_outputs/Fit_Prediction/Fit/M3_MS_hard/M3_MS_hard_pbhat_BIC.RData")
M3_BIC <- BIC


#Obtaining B_12
bf_12 <- exp(0.5*(M2_BIC-M1_BIC))
log10(1/bf_12) #used to get an overall measure of evidence

#Obtaining B_13
bf_13 <- exp(0.5*(M3_BIC-M1_BIC))
log10(1/bf_13) #used to get an overall measure of evidence


#Obtaining B_23
bf_23 <- exp(0.5*(M3_BIC-M2_BIC))
mean(bf_23)
log10(1/bf_23) #used to get an overall measure of evidence
hist(log10(1/bf_23))

