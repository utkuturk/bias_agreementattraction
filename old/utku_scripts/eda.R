
library(languageR)
library(tidyverse)
library(gdata)
library(MASS)
library(magrittr)
library(ggplot2)

fname_data_out <- "../workspace_exp3/exp_data.rds"

df <- readRDS(file = fname_data_out)
df %>% group_by(exp_bias) %>% summarize(N = length(unique(subject)))


df %<>% mutate(ResponseCorrect = (response_yes == (grammatical == "grammatical") ) )
df_nonna <- df %>% subset(!is.na(response_yes))
avg_clean <- data.frame()
avg_clean <- df_nonna %>% 
  plyr::ddply(c("exp_bias"), function(df) {
    df %>% se_cousineau(n_conditions = 4, subject, DV = response_yes, 
                        group = c("experiment", "grammatical", "attractor_num", "exp_bias"), 
                        is_proportion = TRUE)
  })


avg_exp <- avg_clean %>%  subset(is.na(avg_clean$attractor_num) | avg_clean$experiment != "filler")
avg_fillers <- avg_clean %>%  subset(avg_clean$source == "filler")


pd <- position_dodge(0.1)
p_avg_resp <- avg_exp %>%
  ggplot(aes(grammatical, M, #linetype = attractor_num, 
             color = attractor_num, group = attractor_num)) + 
  geom_point(position = pd) + geom_line(position = pd) + 
  facet_wrap(~exp_bias)

p_avg_resp <- p_avg_resp + geom_errorbar(aes(ymin = M - 1.96*SE, ymax = M + 1.96*SE), width = 0.1, position = pd)

p_avg_resp <- p_avg_resp + geom_line(data = avg_fillers) + 
                             geom_point(data = avg_fillers) 

p_avg_resp <- p_avg_resp + theme( strip.background = element_rect(fill="white") ) + theme_bw() + xlab("") + ylab("Percentage 'acceptable'")
p_avg_resp <- p_avg_resp + scale_y_continuous(labels=scales::percent)#, breaks = c(0, .25, .5, .75, 1))
p_avg_resp <- p_avg_resp + theme_bw()
p_avg_resp <- p_avg_resp + scale_color_discrete(name = "Attractor Number") + 
  scale_x_discrete("", labels = c("Grammatical \n Singular Verb", "Ungrammatical \n Plural Verb")) +
  theme(text = element_text(size=14))

ggsave(file="test23.pdf", plot = p_avg_resp, dpi = 300, width = 12, height = 5)


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
#x$cBiasGram <- ifelse(x$exp_bias == "grammatical", .5, -.5)
x$exp_bias %<>% as.factor()
contrasts(x$exp_bias) <- "contr.helmert"


summary( glm(response_yes ~ 1 + cGram, family = binomial("probit"), data = x %>% subset(!is.na(response_yes))) )

#summary( glm(response_yes ~ 1 + cBiasGram * cVerbPl * cGram, family = binomial("probit"), data = x %>% subset(!is.na(response_yes))) )
summary( glm(response_yes ~ 1 + exp_bias * cVerbPl * cGram, family = binomial("probit"), data = x %>% subset(!is.na(response_yes))) )


x$c2Gram <- ifelse(x$grammatical == "gram", 1, 0)

coef(summary( glm(response_yes ~ 1 + cVerbPl * c2Gram, family = binomial("probit"), data = x %>% subset(!is.na(response_yes))) ))


with(x, tapply(response_yes, list(verb_num, grammatical), function(x) mean(x, na.rm=T))) %>% round(3)
