
library(languageR)
library(tidyverse)
library(gdata)
library(MASS)
library(magrittr)
library(ggplot2)

source("scripts/functions.R")



#fname_gb <- "../Data/Experiment3_gb/results/results"
#fname_ugb <- "../Data/Experiment3_ugb/results/results"
#fname_neutral <- "../Data/Experiment3_neutral/results/results"
fname_data_out <- "workspace/exp_data.rds"

source("scripts/grammaticalbias_prepare.R")
source("scripts/ungrammaticalbias_prepare.R")
source("scripts/nogrammaticalbiasintroduction_prepare.R")

data_grammaticalBias_nobadpractice$exp_bias <- "grammatical"
data_ungrammaticalBias_nobadpractice$exp_bias <- "ungrammatical"
data_noBiasinstruction_nobadpractice$exp_bias <- "noInstruction"
data <- bind_rows(data_grammaticalBias_nobadpractice, 
                  data_ungrammaticalBias_nobadpractice, 
                  data_noBiasinstruction_nobadpractice)

saveRDS(data, file = fname_data_out)
