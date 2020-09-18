data_gb <- read.csv("../Data/Experiment3_gb/results/results", 
                 header = F, 
                 comment.char = "#", 
                 encoding = "UTF-8" , 
                 col.names = paste0("V",seq_len(11)), 
                 fill = TRUE, 
                 stringsAsFactors = FALSE)
colnames(data_gb) = c("Time", "MD5", "ControllerType", "SentenceNoInStimFile", "Element", "exp_condition", "item", "Sentence", "Question","Answer", "RT")

subject_id <- with(data_gb, { as.integer(as.factor(paste(Time, MD5))) })
data_gb$subject <- sprintf("S[%d]", subject_id + 0)

df_forms <- data_gb %>% subset(ControllerType != "DashedAcceptabilityJudgment" ) %>% gdata::drop.levels()
data_gb %<>% subset(ControllerType == "DashedAcceptabilityJudgment")

age <- df_forms %>% dplyr::filter(Sentence == "age") %>% 
  dplyr::select(subject, age = Question)
natturk <- df_forms %>% dplyr::filter(Sentence == "natturk") %>% 
  dplyr::select(subject, natturk = Question)
natturk$natturk <- ifelse(natturk$natturk == "male", "nat_turk", "nat_non_turk")
forms <- dplyr::left_join(age, natturk, by = "subject")

stopifnot( nrow(data_gb) %% 2 == 0 )
rows_stim <- data_gb[c(T,F),]
rows_resp <- data_gb[c(F,T),]
stopifnot( all(is.na( rows_stim$RT )) )

data_gb <- rows_resp %>% left_join(forms) %>% 
  dplyr::select(-MD5, -Time, -ControllerType, -Sentence, -Element) %>%
  dplyr::rename(ResponseCorrect=Answer, Response=Question) %>%
  dplyr::select(-ResponseCorrect)
data_gb %<>% group_by(subject) %>% mutate(trial_no = seq(subject))
data_gb %<>% mutate( late_response = (Response == "NULL"), Response = ifelse(late_response, NA, as.character(Response)) )

responses <- c(yes="İYİ (P'ye basınız)", no="KÖTÜ (Q'ya basınız)")
data_gb$Response %<>% as.character() %>% enc2native()
stopifnot( all(data_gb$Response %in% responses | is.na(data_gb$Response) ) )

data_gb$response_yes <- ifelse(grepl("P'ye",data_gb$Response) , T, 
                            ifelse(grepl("Q'ya",data_gb$Response) , F, NA))
print( with(data_gb, table(Response, response_yes)) )
data_gb %<>% dplyr::select(-Response)

data_gb %<>% dplyr::select(-SentenceNoInStimFile) %>% 
  subset(natturk == "nat_turk") %>% 
  subset(exp_condition != "practice") %>% 
  left_join(conditions_info, by = "exp_condition")

