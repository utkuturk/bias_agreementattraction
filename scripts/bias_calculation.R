# Libraries

library(fst) # for reading data files
library(magrittr) # for pipes
library(dplyr)
library(ggplot2)

# Read Data
data_ungrammaticalityBias <- read_fst("exp1_ungrammatical_bias.fst")
data_grammaticalityBias <- read_fst("exp2_grammatical_bias.fst")







# Prepare Data
##  for filler analysis
data_ungrammaticalityBias_nonna_nofillers <- 
  data_ungrammaticalityBias %>% 
    subset(!is.na(response_yes) & experiment == "filler")

data_grammaticalityBias_nonna_nofillers <- 
  data_grammaticalityBias %>% 
    subset(!is.na(response_yes) & experiment == "filler")


##  for "whole" analysis
data_ungrammaticalityBias_nonna <- 
  data_ungrammaticalityBias %>% 
    subset(!is.na(response_yes))

data_grammaticalityBias_nonna <- 
  data_grammaticalityBias %>% 
    subset(!is.na(response_yes))





# Prepare variables for the function

##  Fillers - Ung Bias
###  hit rates
hitRates_ung_fillers <-
data_ungrammaticalityBias_nonna_nofillers %>% 
  group_by(subject) %>% 
    filter(exp_condition == "filler_g") %>% 
      summarize(
        hitRate =  mean(ResponseCorrect),
      ) 

###  false alarms
falseAlarms_ung_fillers <-
data_ungrammaticalityBias_nonna_nofillers %>% 
  group_by(subject) %>% 
    filter(exp_condition == "filler_ung") %>% 
      summarize(
        falseAlarm =  mean(response_yes),
      ) 


###  start working on zscore dataframe
zscores_ung_fillers <- 
  full_join(hitRates_ung_fillers, 
            falseAlarms_ung_fillers, 
            by = "subject")

### zscore calculation
zscores_ung_fillers <- 
  zscores_ung_fillers %>% 
            mutate(
              zscore_hitRates = (hitRate - mean(hitRate))/sd(hitRate),
              zscore_falseAlarms = (falseAlarm - mean(falseAlarm))/sd(falseAlarm)
            )

### bias calculation
biases_ung_fillers <- 
  zscores_ung_fillers %>% 
    mutate(
      bias = -1 * ( ( zscore_hitRates + zscore_falseAlarms ) / 2 )
    )

### plot
biases_ung_fillers %>% ggplot(aes(bias, subject)) + 
  geom_point()









##  Fillers - Gram Bias
###  hit rates
hitRates_g_fillers <-
  data_grammaticalityBias_nonna_nofillers %>% 
  group_by(subject) %>% 
  filter(exp_condition == "filler_g") %>% 
  summarize(
    hitRate =  mean(ResponseCorrect),
  )

### false alarms
falseAlarms_g_fillers <-
  data_grammaticalityBias_nonna_nofillers %>% 
  group_by(subject) %>% 
  filter(exp_condition == "filler_ung") %>% 
  summarize(
    falseAlarm =  mean(response_yes),
  ) 

###  start working on zscore dataframe
zscores_g_fillers <- 
  full_join(hitRates_g_fillers, 
            falseAlarms_g_fillers, 
            by = "subject")

### zscore calculation
zscores_g_fillers <- 
  zscores_g_fillers %>% 
    mutate(
      zscore_hitRates = (hitRate - mean(hitRate))/sd(hitRate),
      zscore_falseAlarms = (falseAlarm - mean(falseAlarm))/sd(falseAlarm)
    )

### bias calculation
biases_g_fillers <- 
  zscores_g_fillers %>% 
    mutate(
      bias = -1 * ((zscore_hitRates + zscore_falseAlarms)/2)
    )

### plot
biases_g_fillers %>% ggplot(aes(bias, subject)) + 
  geom_point() + geom_line()












## Exp-wide data

### prepare means and CI
ungBias_fillers_ExpBias <- 
biases_ung_fillers %>%  
  summarize(p_bias = mean(bias), 
          N = length(subject),
          CI = 1.96*(sd(bias)/sqrt(length(bias)))) 
ungBias_fillers_ExpBias$exp <- "ungBias"

gBias_fillers_ExpBias <- 
  biases_g_fillers %>%  
  summarize(p_bias = mean(bias), 
            N = length(subject),
            CI = 1.96*(sd(bias)/sqrt(length(bias)))) 
gBias_fillers_ExpBias$exp <- "gBias"


### binding
filler_biases <-
  rbind(gBias_fillers_ExpBias, 
        ungBias_fillers_ExpBias)


### plot
filler_biases %>% 
  ggplot(aes(exp, p_bias)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = p_bias-CI, 
                    ymax = p_bias+CI))



















##  Whole - Ung Bias

hitRates_ung_whole <-
  data_ungrammaticalityBias_nonna %>% 
  group_by(subject) %>% 
  filter(grammatical == "gram") %>% 
  summarize(
    hitRate =  mean(ResponseCorrect),
  ) 
falseAlarms_ung_whole <-
  data_ungrammaticalityBias_nonna %>% 
  group_by(subject) %>% 
  filter(grammatical == "ungram") %>% 
  summarize(
    falseAlarm =  mean(response_yes),
  ) 
zscores_ung_whole <- 
  full_join(hitRates_ung_whole, 
            falseAlarms_ung_whole, by = "subject")
zscores_ung_whole <- zscores_ung_whole %>% 
  mutate(
    zscore_hitRates = (hitRate - mean(hitRate))/sd(hitRate),
    zscore_falseAlarms = (falseAlarm - mean(falseAlarm))/sd(falseAlarm)
  )
biases_ung_whole <- zscores_ung_whole %>% 
  mutate(
    bias = -1 * ((zscore_hitRates + zscore_falseAlarms)/2)
  )
biases_ung_whole %>% ggplot(aes(bias, subject)) + 
  geom_point()



##  Whole - Gram Bias

hitRates_g_whole <-
  data_grammaticalityBias_nonna %>% 
  group_by(subject) %>% 
  filter(grammatical == "gram") %>% 
  summarize(
    hitRate =  mean(ResponseCorrect),
  ) 
falseAlarms_g_whole <-
  data_grammaticalityBias_nonna %>% 
  group_by(subject) %>% 
  filter(grammatical == "ungram") %>% 
  summarize(
    falseAlarm =  mean(response_yes),
  ) 
zscores_g_whole <- 
  full_join(hitRates_g_whole, 
            falseAlarms_g_whole, by = "subject")
zscores_g_whole <- zscores_g_whole %>% 
  mutate(
    zscore_hitRates = (hitRate - mean(hitRate))/sd(hitRate),
    zscore_falseAlarms = (falseAlarm - mean(falseAlarm))/sd(falseAlarm)
  )
biases_g_whole <- zscores_g_whole %>% 
  mutate(
    bias = -1 * ((zscore_hitRates + zscore_falseAlarms)/2)
  )
biases_g_whole %>% ggplot(aes(bias, subject)) + 
  geom_point() + geom_line()









## Exp-wide data
ungBias_whole_ExpBias <- 
  biases_ung_whole %>%  
  summarize(p_bias = mean(bias), 
            N = length(subject),
            CI = 1.96*(sd(bias)/sqrt(length(bias)))) 
ungBias_whole_ExpBias$exp <- "ungBias"

gBias_whole_ExpBias <- 
  biases_g_whole %>%  
  summarize(p_bias = mean(bias), 
            N = length(subject),
            CI = 1.96*(sd(bias)/sqrt(length(bias)))) 
gBias_whole_ExpBias$exp <- "gBias"

whole_biases <-
  rbind(gBias_whole_ExpBias, ungBias_whole_ExpBias)

whole_biases %>% 
  ggplot(aes(exp, p_bias)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = p_bias-CI, 
                    ymax = p_bias+CI))




