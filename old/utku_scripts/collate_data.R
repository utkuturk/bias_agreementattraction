
library(languageR)
library(tidyverse)
library(gdata)
library(MASS)
library(magrittr)
library(ggplot2)

source("./functions.R")

conditions_info <- data.frame(exp_condition = c("condition_a", "condition_b", "condition_c", "condition_d", "filler_ung", "filler_g"),
                              experiment =    c("AgrAttr",     "AgrAttr",     "AgrAttr",     "AgrAttr",     "filler",     "filler"),
                              condition =     c("a",           "b",           "c",           "d",           "ung",        "g"),
                              grammatical =   c("ungram",      "gram",        "ungram",      "gram",        "ungram",     "gram"),
                              verb_num =      c("pl",          "sg",          "pl",          "sg",          "sg",         "pl"),
                              attractor_num = c("pl",          "pl",          "sg",          "sg",          NA,           NA),
                              stringsAsFactors = FALSE)


fname_gb <- "../Data/Experiment3_gb/results/results"
fname_ugb <- "../Data/Experiment3_ugb/results/results"
fname_neutral <- "../Data/Experiment3_neutral/results/results"
fname_data_out <- "../workspace_exp3/exp_data.rds"

source("./exp3_gb_prepare.R")
source("./exp3_ugb_prepare.R")
source("./exp3_n_prepare.R")

data_gb$exp_bias <- "grammatical"
data_ugb$exp_bias <- "ungrammatical"
data_n$exp_bias <- "neutral"
data <- bind_rows(data_gb, data_ugb, data_n)

saveRDS(data, file = fname_data_out)
