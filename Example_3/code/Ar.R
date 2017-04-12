# Clean environment
rm(list = ls())

# Set options and load libraries and functions
options(width=120)
if(!require(knitr))
  install.packages("knitr",dependencies=TRUE)
library(knitr)

if(!require(parallel))
  install.packages("parallel",dependencies=TRUE)
library(parallel)
options(mc.cores = parallel::detectCores())

if(!require(rstan))
  install.packages("rstan",dependencies=TRUE)
library(rstan)
rstan_options(auto_write = TRUE)

source('code/models/phys_model_Ar.R')
source('code/functions/gen_synth_data.R')
source('code/functions/plot_funcs.R')

# Model functions for stan ####
phys_model = 
  paste0(
    readLines(con='code/models/phys_model_Ar.stan'),
    collapse='\n'
  )

# Sampling options
nb_warmup = 1000
nb_iter   = 5000
nb_chains = 4

# Ar ###############################################################
case= 'Ar'
inadequacy = FALSE
shift = TRUE

list_meth=c('WLS','VarInf_Rb','ABC','Margin',
            'Margin1','Margin2','Margin3')
list_legends=c(FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE)
names(list_legends)= list_meth

## Define plot limits
eps_lim=c(130,160)     # Limits for epsilon plots
sig_lim=c(3.28,3.34)   # Limits for sigma plots
res_lim=c(-1,1) * 0.5  # Limits for residuals plots

## Run simulations for the current case
source('code/functions/simul_all.R')

# Session Info ####
sink(file='session_info.txt')
print(sessionInfo())
sink()
