
library(languageR)
library(tidyverse)
library(gdata)
library(MASS)
library(magrittr)
library(ggplot2)

fname_data_out <- "workspace/exp_data.rds"
source("scripts/functions.R")

df <- readRDS(file = fname_data_out)
df %>% group_by(exp_bias) %>% summarize(N = length(unique(subject)))
df %<>% mutate(ResponseCorrect = (response_yes == (grammatical == "gram") ) )

# compute by-subject percentages of 'yes' responses, and average RTs 
avg_by_subj <- df %>%
  group_by(subject, exp_bias, condition, 
           grammatical, verb_num, attractor_num) %>%
  summarize(avRT = mean(RT), 
            p_yes = mean(response_yes, na.rm = T), 
            N = sum(!is.na(response_yes))  )

avg_by_subj_wide <- avg_by_subj %>% 
  mutate(expcond = paste(experiment, condition, sep="_")) %>% 
  ungroup() %>%
  dplyr::select(-experiment, -condition, -avRT, -N,
                -grammatical, -verb_num, -attractor_num) %>%
  tidyr::spread(expcond, p_yes) %>% 
  mutate(delta_dc = AgrAttr_d - AgrAttr_c)


avg_clean <- list()
avg_clean$resp <- df %>% 
  plyr::ddply(c("exp_bias"), function(df) {
    df %>% se_cousineau(n_conditions = 4, subject, DV = response_yes, 
                        group = c("experiment", "exp_bias", "grammatical", "attractor_num"), 
                        is_proportion = TRUE)
  })

avg_clean$rt <- df %>%
  plyr::ddply(c("exp_bias"), function(df) {
    df %>% se_cousineau(n_conditions = 4, subject, DV = RT, 
                        group = c("experiment", "exp_bias", "grammatical", "attractor_num"), 
                        is_proportion = FALSE)
  })

avg_clean$rt_correct <- df %>% subset(ResponseCorrect) %>%
  plyr::ddply(c("exp_bias"), function(df) {
    df %>% se_cousineau(n_conditions = 4, subject, DV = RT, 
                        group = c("experiment", "exp_bias", "grammatical", "attractor_num"), 
                        is_proportion = FALSE)
  })

avg_exp <- avg_clean %>% lapply(function(df) { df %>% subset(is.na(source) | experiment != "filler") })
avg_fillers <- avg_clean %>% lapply(function(df) { df %>% subset(experiment == "filler") })

pd <- position_dodge(0.1)
p_avg_resp <- avg_exp$resp %>%
  ggplot(aes(grammatical, M, #linetype = attractor_num, 
             color = attractor_num, group = attractor_num)) + 
  geom_point(position = pd) + geom_line(position = pd) + 
  facet_wrap(~exp_bias)

p_avg_resp <- p_avg_resp + geom_errorbar(aes(ymin = M - 1.96*SE, ymax = M + 1.96*SE), 
                                         width = 0.1, position = pd)

p_avg_resp <- p_avg_resp + geom_line(data = avg_fillers$resp) + 
  geom_point(data = avg_fillers$resp) 

p_avg_resp <- p_avg_resp + theme( strip.background = element_rect(fill="white") ) +
  theme_bw() + xlab("") + ylab("Percentage 'acceptable'")
p_avg_resp <- p_avg_resp + scale_y_continuous(labels=scales::percent)#, breaks = c(0, .25, .5, .75, 1))
p_avg_resp <- p_avg_resp + theme_bw()
p_avg_resp <- p_avg_resp + scale_color_discrete(name = "Attractor Number")



####BYSUBJECT------

# compute by-subject averages
avgs_bysubj <- 
  df %>% group_by(experiment, condition, exp_condition, grammatical, attractor_num, subject, exp_bias) %>% 
  summarize(p_yes = mean(response_yes, na.rm = T) ) 
avgs_bysubj$attractor_num[is.na(avgs_bysubj$attractor_num)] <- "filler"
c <- with(avgs_bysubj, { as.character(as.factor(paste(attractor_num, grammatical))) })
avgs_bysubj$c <- c  

avgs_bysubj_wide <- 
  df %>% group_by(experiment, condition, exp_condition, grammatical, attractor_num, subject,exp_bias) %>% 
  summarize(p_yes = mean(response_yes, na.rm = T) ) %>%
  ungroup() %>% 
  dplyr::select(exp_condition, subject, p_yes,exp_bias) %>%
  tidyr::pivot_wider(names_from = exp_condition, values_from = p_yes) %>%
  mutate(delta_attr = condition_b - condition_a,
         delta_noattr = condition_d - condition_c,
         delta_fillers = filler_g - filler_ung)

base <- ggplot(avgs_bysubj, aes(x = c, y = p_yes)) + facet_wrap(~exp_bias)
base + geom_boxplot()
# violin plot with mean points (+/- SD)
base + geom_violin(trim = T) + 
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), 
               geom="pointrange", color = "red")
