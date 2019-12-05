
library(languageR)
library(tidyverse)
library(gdata)
library(MASS)
library(magrittr)
library(ggplot2)

fname_data_out <- "../workspace_exp3/exp_data.rds"

df <- readRDS(file = fname_data_out)
df %>% group_by(exp_bias) %>% summarize(N = length(unique(subject)))

# overall averages
df %>% group_by(experiment, condition, grammatical, attractor_num, exp_bias) %>% 
              summarize(p_yes = mean(response_yes, na.rm = T), N = sum( !is.na(response_yes) ), N_NA = sum( is.na(response_yes) ) ) %>%
        ggplot(aes(grammatical, p_yes, color = attractor_num, group = paste(attractor_num, experiment) )) + geom_point() + geom_line() +
        facet_wrap(~exp_bias) + scale_y_continuous(limits = c(0,1))


# compute by-subject averages
avgs_bysubj_wide <- 
  df %>% group_by(experiment, condition, exp_condition, grammatical, attractor_num, subject) %>% 
  summarize(p_yes = mean(response_yes, na.rm = T) ) %>%
  ungroup() %>% 
  dplyr::select(exp_condition, subject, p_yes) %>%
  tidyr::pivot_wider(names_from = exp_condition, values_from = p_yes) %>%
  mutate(delta_attr = condition_b - condition_a,
         delta_noattr = condition_d - condition_c,
         delta_fillers = filler_g - filler_ung)


# look at determinants of magnitude of the attraction effect
library(rpart)
library(rattle)

m <- rpart(delta_attr ~ . - subject, data = avgs_bysubj_wide,
           control = rpart.control(maxdepth = 1))
fancyRpartPlot(m)


# determine bad subjects
bad_subjects <- avgs_bysubj_wide %>% subset(delta_fillers < .4 | delta_noattr < .4)
df_bad_subjects <- df %>% subset(subject %in% bad_subjects$subject)
df <- df %>% subset(!subject %in% bad_subjects$subject)

# averages by participant: bad participant
df %>%
  group_by(experiment, condition, exp_condition, grammatical, attractor_num, subject) %>% 
  summarize(p_yes = mean(response_yes, na.rm = T) ) %>% 
  ggplot(aes(grammatical, p_yes, color = attractor_num, group = paste(attractor_num, experiment) )) + 
  geom_point() + geom_line() + scale_y_continuous(limits = c(0,1)) + facet_wrap(~subject)


# overall averages (after removal of bad subjects)
df %>% group_by(experiment, condition, grammatical, attractor_num, exp_bias) %>% 
  summarize(p_yes = mean(response_yes, na.rm = T), N = sum( !is.na(response_yes) ), N_NA = sum( is.na(response_yes) ) ) %>%
  ggplot(aes(grammatical, p_yes, color = attractor_num, group = paste(attractor_num, experiment) )) + geom_point() + geom_line() +
  facet_wrap(~exp_bias) + scale_y_continuous(limits = c(0,1))


# averages by participant: grammatical
df %>% subset(exp_bias == "grammatical") %>% 
  group_by(experiment, condition, exp_condition, grammatical, attractor_num, subject) %>% 
  summarize(p_yes = mean(response_yes, na.rm = T) ) %>% 
  ggplot(aes(grammatical, p_yes, color = attractor_num, group = paste(attractor_num, experiment) )) + 
  geom_point() + geom_line() + scale_y_continuous(limits = c(0,1)) + facet_wrap(~subject)


# averages by participant: ungrammatical
df %>% subset(exp_bias == "ungrammatical") %>% 
  group_by(experiment, condition, grammatical, attractor_num, subject) %>% 
  summarize(p_yes = mean(response_yes, na.rm = T) ) %>%
  ggplot(aes(grammatical, p_yes, color = attractor_num, group = paste(attractor_num, experiment) )) + geom_point() + geom_line() +
  scale_y_continuous(limits = c(0,1)) + facet_wrap(~subject)


x <- subset(df, ((is.na(attractor_num) | attractor_num != "pl" )) )
x %>% group_by(exp_condition, experiment, condition, grammatical, attractor_num, verb_num) %>% summarize(p_yes = mean(response_yes, na.rm = T))

x$cGram <- ifelse(x$grammatical == "gram", .5, -.5)
x$cVerbPl <- ifelse(x$verb_num == "pl", .5, -.5)
x$cBiasGram <- ifelse(x$exp_bias == "grammatical", .5, -.5)



summary( glm(response_yes ~ 1 + cGram, family = binomial("probit"), data = x %>% subset(!is.na(response_yes))) )

summary( glm(response_yes ~ 1 + cBiasGram * cVerbPl * cGram, family = binomial("probit"), data = x %>% subset(!is.na(response_yes))) )


x$c2Gram <- ifelse(x$grammatical == "gram", 1, 0)

coef(summary( glm(response_yes ~ 1 + cVerbPl * c2Gram, family = binomial("probit"), data = x %>% subset(!is.na(response_yes))) ))


with(x, tapply(response_yes, list(verb_num, grammatical), function(x) mean(x, na.rm=T))) %>% round(3)
